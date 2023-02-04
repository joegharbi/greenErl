/* Read the RAPL registers on recent (>sandybridge) Intel processors	*/
/*									*/
/* There are currently three ways to do this:				*/
/*	1. Read the MSRs directly with /dev/cpu/??/msr			*/
/*	2. Use the perf_event_open() interface				*/
/*	3. Read the values from the sysfs powercap interface		*/
/*									*/
/* MSR Code originally based on a (never made it upstream) linux-kernel	*/
/*	RAPL driver by Zhang Rui <rui.zhang@intel.com>			*/
/*	https://lkml.org/lkml/2011/5/26/93				*/
/* Additional contributions by:						*/
/*	Romain Dolbeau -- romain @ dolbeau.org				*/
/*									*/
/* For raw MSR access the /dev/cpu/??/msr driver must be enabled and	*/
/*	permissions set to allow read access.				*/
/*	You might need to "modprobe msr" before it will work.		*/
/*									*/
/* perf_event_open() support requires at least Linux 3.14 and to have	*/
/*	/proc/sys/kernel/perf_event_paranoid < 1			*/
/*									*/
/* the sysfs powercap interface got into the kernel in 			*/
/*	2d281d8196e38dd (3.13)						*/
/*									*/
/* Compile with:   gcc -O2 -Wall -o rapl-read rapl-read.c -lm		*/
/*									*/
/* Vince Weaver -- vincent.weaver @ maine.edu -- 11 September 2015	*/
/*									*/

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include <inttypes.h>
#include <unistd.h>
#include <math.h>
#include <string.h>
#include <sys/syscall.h>
#include <linux/perf_event.h>

#include "erlang_comm.h"

#define MSR_RAPL_POWER_UNIT 0x606

/*
 * Platform specific RAPL Domains.
 * Note that PP1 RAPL Domain is supported on 062A only
 * And DRAM RAPL Domain is supported on 062D only
 */
/* Package RAPL Domain */
#define MSR_PKG_RAPL_POWER_LIMIT 0x610
#define MSR_PKG_ENERGY_STATUS 0x611
#define MSR_PKG_PERF_STATUS 0x613
#define MSR_PKG_POWER_INFO 0x614

/* PP0 RAPL Domain */
#define MSR_PP0_POWER_LIMIT 0x638
#define MSR_PP0_ENERGY_STATUS 0x639
#define MSR_PP0_POLICY 0x63A
#define MSR_PP0_PERF_STATUS 0x63B

/* PP1 RAPL Domain, may reflect to uncore devices */
#define MSR_PP1_POWER_LIMIT 0x640
#define MSR_PP1_ENERGY_STATUS 0x641
#define MSR_PP1_POLICY 0x642

/* DRAM RAPL Domain */
#define MSR_DRAM_POWER_LIMIT 0x618
#define MSR_DRAM_ENERGY_STATUS 0x619
#define MSR_DRAM_PERF_STATUS 0x61B
#define MSR_DRAM_POWER_INFO 0x61C

/* PSYS RAPL Domain */
#define MSR_PLATFORM_ENERGY_STATUS 0x64d

/* RAPL UNIT BITMASK */
#define POWER_UNIT_OFFSET 0
#define POWER_UNIT_MASK 0x0F

#define ENERGY_UNIT_OFFSET 0x08
#define ENERGY_UNIT_MASK 0x1F00

#define TIME_UNIT_OFFSET 0x10
#define TIME_UNIT_MASK 0xF000

static int open_msr(int core)
{

	char msr_filename[BUFSIZ];
	int fd;

	sprintf(msr_filename, "/dev/cpu/%d/msr", core);
	fd = open(msr_filename, O_RDONLY);
	if (fd < 0)
	{
		if (errno == ENXIO)
		{
			fprintf(stderr, "rdmsr: No CPU %d\n", core);
			exit(2);
		}
		else if (errno == EIO)
		{
			fprintf(stderr, "rdmsr: CPU %d doesn't support MSRs\n",
					core);
			exit(3);
		}
		else
		{
			perror("rdmsr:open");
			fprintf(stderr, "Trying to open %s\n", msr_filename);
			exit(127);
		}
	}

	return fd;
}

static long long read_msr(int fd, int which)
{

	uint64_t data;

	if (pread(fd, &data, sizeof data, which) != sizeof data)
	{
		perror("rdmsr:pread");
		exit(127);
	}

	return (long long)data;
}

#define CPU_SANDYBRIDGE 42
#define CPU_SANDYBRIDGE_EP 45
#define CPU_IVYBRIDGE 58
#define CPU_IVYBRIDGE_EP 62
#define CPU_HASWELL 60
#define CPU_HASWELL_ULT 69
#define CPU_HASWELL_GT3E 70
#define CPU_HASWELL_EP 63
#define CPU_BROADWELL 61
#define CPU_BROADWELL_GT3E 71
#define CPU_BROADWELL_EP 79
#define CPU_BROADWELL_DE 86
#define CPU_SKYLAKE 78
#define CPU_SKYLAKE_HS 94
#define CPU_SKYLAKE_X 85
#define CPU_KNIGHTS_LANDING 87
#define CPU_KNIGHTS_MILL 133
#define CPU_KABYLAKE_MOBILE 142
#define CPU_KABYLAKE 158
#define CPU_ATOM_SILVERMONT 55
#define CPU_ATOM_AIRMONT 76
#define CPU_ATOM_MERRIFIELD 74
#define CPU_ATOM_MOOREFIELD 90
#define CPU_ATOM_GOLDMONT 92
#define CPU_ATOM_GEMINI_LAKE 122
#define CPU_ATOM_DENVERTON 95

/* TODO: on Skylake, also may support  PSys "platform" domain,	*/
/* the whole SoC not just the package.				*/
/* see dcee75b3b7f025cc6765e6c92ba0a4e59a4d25f4			*/

static int detect_cpu(void)
{

	FILE *fff;

	int family, model = -1;
	char buffer[BUFSIZ], *result;
	char vendor[BUFSIZ];

	fff = fopen("/proc/cpuinfo", "r");
	if (fff == NULL)
		return -1;

	while (1)
	{
		result = fgets(buffer, BUFSIZ, fff);
		if (result == NULL)
			break;

		if (!strncmp(result, "vendor_id", 8))
		{
			sscanf(result, "%*s%*s%s", vendor);

			if (strncmp(vendor, "GenuineIntel", 12))
			{
				// printf("%s not an Intel chip\n",vendor);
				return -1;
			}
		}

		if (!strncmp(result, "cpu family", 10))
		{
			sscanf(result, "%*s%*s%*s%d", &family);
			if (family != 6)
			{
				// printf("Wrong CPU family %d\n",family);
				return -1;
			}
		}

		if (!strncmp(result, "model", 5))
		{
			sscanf(result, "%*s%*s%d", &model);
		}
	}

	fclose(fff);

	// printf("Found ");

	switch (model)
	{
	case CPU_SANDYBRIDGE:
		// printf("Sandybridge");
		break;
	case CPU_SANDYBRIDGE_EP:
		// printf("Sandybridge-EP");
		break;
	case CPU_IVYBRIDGE:
		// printf("Ivybridge");
		break;
	case CPU_IVYBRIDGE_EP:
		// printf("Ivybridge-EP");
		break;
	case CPU_HASWELL:
	case CPU_HASWELL_ULT:
	case CPU_HASWELL_GT3E:
		// printf("Haswell");
		break;
	case CPU_HASWELL_EP:
		// printf("Haswell-EP");
		break;
	case CPU_BROADWELL:
	case CPU_BROADWELL_GT3E:
		// printf("Broadwell");
		break;
	case CPU_BROADWELL_EP:
		// printf("Broadwell-EP");
		break;
	case CPU_SKYLAKE:
	case CPU_SKYLAKE_HS:
		// printf("Skylake");
		break;
	case CPU_SKYLAKE_X:
		// printf("Skylake-X");
		break;
	case CPU_KABYLAKE:
	case CPU_KABYLAKE_MOBILE:
		// printf("Kaby Lake");
		break;
	case CPU_KNIGHTS_LANDING:
		// printf("Knight's Landing");
		break;
	case CPU_KNIGHTS_MILL:
		// printf("Knight's Mill");
		break;
	case CPU_ATOM_GOLDMONT:
	case CPU_ATOM_GEMINI_LAKE:
	case CPU_ATOM_DENVERTON:
		// printf("Atom");
		break;
	default:
		// printf("Unsupported model %d\n",model);
		model = -1;
		break;
	}

	// printf(" Processor type\n");

	return model;
}

#define MAX_CPUS 1024
#define MAX_PACKAGES 16

static int total_cores = 0, total_packages = 0;
static int package_map[MAX_PACKAGES];

static int detect_packages(void)
{

	char filename[BUFSIZ];
	FILE *fff;
	int package;
	int i;

	for (i = 0; i < MAX_PACKAGES; i++)
		package_map[i] = -1;

	// printf("\t");
	for (i = 0; i < MAX_CPUS; i++)
	{
		sprintf(filename, "/sys/devices/system/cpu/cpu%d/topology/physical_package_id", i);
		fff = fopen(filename, "r");
		if (fff == NULL)
			break;
		fscanf(fff, "%d", &package);
		// printf("%d (%d)",i,package);
		// if (i%8==7) printf("\n\t"); else printf(", ");
		fclose(fff);

		if (package_map[package] == -1)
		{
			total_packages++;
			package_map[package] = i;
		}
	}

	// printf("\n");

	total_cores = i;

	// printf("\tDetected %d cores in %d packages\n\n",
	//	total_cores,total_packages);

	return 0;
}

/*******************************/
/* MSR code                    */
/*******************************/
int fd_msr;
long long result_msr;
double power_units, time_units;
double cpu_energy_units[MAX_PACKAGES], dram_energy_units[MAX_PACKAGES];
double package_before = 0, package_after = 0;
double pp0_before = 0, pp0_after = 0;
double pp1_before = 0, pp1_after = 0;
double dram_before = 0, dram_after = 0;
double psys_before = 0, psys_after = 0;
int dram_avail = 0, pp0_avail = 0, pp1_avail = 0, psys_avail = 0;
int different_units = 0;
double msr_results[5];

static int rapl_msr_before(int core, int cpu_model)
{
	int j;
	package_before = 0, package_after = 0;
	pp0_before = 0, pp0_after = 0;
	pp1_before = 0, pp1_after = 0;
	dram_before = 0, dram_after = 0;
	psys_before = 0, psys_after = 0;

	if (cpu_model < 0)
	{
		return -1;
	}

	switch (cpu_model)
	{

	case CPU_SANDYBRIDGE_EP:
	case CPU_IVYBRIDGE_EP:
		pp0_avail = 1;
		pp1_avail = 0;
		dram_avail = 1;
		different_units = 0;
		psys_avail = 0;
		break;

	case CPU_HASWELL_EP:
	case CPU_BROADWELL_EP:
	case CPU_SKYLAKE_X:
		pp0_avail = 1;
		pp1_avail = 0;
		dram_avail = 1;
		different_units = 1;
		psys_avail = 0;
		break;

	case CPU_KNIGHTS_LANDING:
	case CPU_KNIGHTS_MILL:
		pp0_avail = 0;
		pp1_avail = 0;
		dram_avail = 1;
		different_units = 1;
		psys_avail = 0;
		break;

	case CPU_SANDYBRIDGE:
	case CPU_IVYBRIDGE:
		pp0_avail = 1;
		pp1_avail = 1;
		dram_avail = 0;
		different_units = 0;
		psys_avail = 0;
		break;

	case CPU_HASWELL:
	case CPU_HASWELL_ULT:
	case CPU_HASWELL_GT3E:
	case CPU_BROADWELL:
	case CPU_BROADWELL_GT3E:
	case CPU_ATOM_GOLDMONT:
	case CPU_ATOM_GEMINI_LAKE:
	case CPU_ATOM_DENVERTON:
		pp0_avail = 1;
		pp1_avail = 1;
		dram_avail = 1;
		different_units = 0;
		psys_avail = 0;
		break;

	case CPU_SKYLAKE:
	case CPU_SKYLAKE_HS:
	case CPU_KABYLAKE:
	case CPU_KABYLAKE_MOBILE:
		pp0_avail = 1;
		pp1_avail = 1;
		dram_avail = 1;
		different_units = 0;
		psys_avail = 1;
		break;
	}

	for (j = 0; j < total_packages; j++)
	{

		fd_msr = open_msr(package_map[j]);

		/* Calculate the units used */
		result_msr = read_msr(fd_msr, MSR_RAPL_POWER_UNIT);

		power_units = pow(0.5, (double)(result_msr & 0xf));
		cpu_energy_units[j] = pow(0.5, (double)((result_msr >> 8) & 0x1f));
		time_units = pow(0.5, (double)((result_msr >> 16) & 0xf));

		/* On Haswell EP and Knights Landing */
		/* The DRAM units differ from the CPU ones */
		if (different_units)
		{
			dram_energy_units[j] = pow(0.5, (double)16);
			// printf("DRAM: Using %lf instead of %lf\n",
			//	dram_energy_units[j],cpu_energy_units[j]);
		}
		else
		{
			dram_energy_units[j] = cpu_energy_units[j];
		}
		close(fd_msr);
	}

	for (j = 0; j < total_packages; j++)
	{

		fd_msr = open_msr(package_map[j]);

		/* Package Energy */
		result_msr = read_msr(fd_msr, MSR_PKG_ENERGY_STATUS);
		package_before += (double)result_msr * cpu_energy_units[j];

		/* PP0 energy */
		/* Not available on Knights* */
		/* Always returns zero on Haswell-EP? */
		if (pp0_avail)
		{
			result_msr = read_msr(fd_msr, MSR_PP0_ENERGY_STATUS);
			pp0_before += (double)result_msr * cpu_energy_units[j];
		}

		/* PP1 energy */
		/* not available on *Bridge-EP */
		if (pp1_avail)
		{
			result_msr = read_msr(fd_msr, MSR_PP1_ENERGY_STATUS);
			pp1_before += (double)result_msr * cpu_energy_units[j];
		}

		/* Updated documentation (but not the Vol3B) says Haswell and	*/
		/* Broadwell have DRAM support too				*/
		if (dram_avail)
		{
			result_msr = read_msr(fd_msr, MSR_DRAM_ENERGY_STATUS);
			dram_before += (double)result_msr * dram_energy_units[j];
		}

		/* Skylake and newer for Psys				*/
		if ((cpu_model == CPU_SKYLAKE) ||
			(cpu_model == CPU_SKYLAKE_HS) ||
			(cpu_model == CPU_KABYLAKE) ||
			(cpu_model == CPU_KABYLAKE_MOBILE))
		{

			result_msr = read_msr(fd_msr, MSR_PLATFORM_ENERGY_STATUS);
			psys_before += (double)result_msr * cpu_energy_units[j];
		}

		close(fd_msr);
	}
}

static int rapl_msr_after(int core, int cpu_model)
{
	int j;

	for (j = 0; j < total_packages; j++)
	{

		fd_msr = open_msr(package_map[j]);

		// printf("\tPackage %d:\n",j);

		result_msr = read_msr(fd_msr, MSR_PKG_ENERGY_STATUS);
		package_after += (double)result_msr * cpu_energy_units[j];

		result_msr = read_msr(fd_msr, MSR_PP0_ENERGY_STATUS);
		pp0_after += (double)result_msr * cpu_energy_units[j];

		/* not available on SandyBridge-EP */
		if (pp1_avail)
		{
			result_msr = read_msr(fd_msr, MSR_PP1_ENERGY_STATUS);
			pp1_after += (double)result_msr * cpu_energy_units[j];
		}

		if (dram_avail)
		{
			result_msr = read_msr(fd_msr, MSR_DRAM_ENERGY_STATUS);
			dram_after += (double)result_msr * dram_energy_units[j];
		}

		if (psys_avail)
		{
			result_msr = read_msr(fd_msr, MSR_PLATFORM_ENERGY_STATUS);
			psys_after += (double)result_msr * cpu_energy_units[j];
		}

		close(fd_msr);
	}
	msr_results[0] = package_after - package_before;
	msr_results[1] = pp0_after - pp0_before;

	if (pp1_avail)
	{
		msr_results[2] = pp1_after - pp1_before;
	}
	else
	{
		msr_results[2] = 0;
	}

	if (dram_avail)
	{
		msr_results[3] = dram_after - dram_before;
	}
	else
	{
		msr_results[3] = 0;
	}

	if (psys_avail)
	{
		msr_results[4] = psys_after - psys_before;
	}
	else
	{
		msr_results[4] = 0;
	}
	// printf("\n");
	// printf("Note: the energy measurements can overflow in 60s or so\n");
	// printf("      so try to sample the counters more often than that.\n\n");

	return 0;
}

static int perf_event_open(struct perf_event_attr *hw_event_uptr,
						   pid_t pid, int cpu, int group_fd, unsigned long flags)
{

	return syscall(perf_event_open, hw_event_uptr, pid, cpu,
				   group_fd, flags);
}

#define NUM_RAPL_DOMAINS 4 // 5

char rapl_domain_names[NUM_RAPL_DOMAINS][30] = {
	"energy-pkg",
	"energy-cores",
	"energy-gpu",
	"energy-ram" //,
				 //"energy-psys"
};

static int check_paranoid(void)
{

	int paranoid_value;
	FILE *fff;

	fff = fopen("/proc/sys/kernel/perf_event_paranoid", "r");
	if (fff == NULL)
	{
		fprintf(stderr, "Error! could not open /proc/sys/kernel/perf_event_paranoid %s\n",
				strerror(errno));

		/* We can't return a negative value as that implies no paranoia */
		return 500;
	}

	fscanf(fff, "%d", &paranoid_value);
	fclose(fff);

	return paranoid_value;
}

int fd_perf[NUM_RAPL_DOMAINS][MAX_PACKAGES];
long long value;
double scale[NUM_RAPL_DOMAINS];
char units[NUM_RAPL_DOMAINS][BUFSIZ];

static int rapl_perf_before(int core)
{

	FILE *fff;
	int type;
	int config[NUM_RAPL_DOMAINS];

	char filename[BUFSIZ];

	struct perf_event_attr attr;

	int i, j;
	int paranoid_value;

	// printf("\nTrying perf_event interface to gather results\n\n");

	fff = fopen("/sys/bus/event_source/devices/power/type", "r");
	if (fff == NULL)
	{
		// printf("\tNo perf_event rapl support found (requires Linux 3.14)\n");
		// printf("\tFalling back to raw msr support\n\n");
		return -1;
	}
	fscanf(fff, "%d", &type);
	fclose(fff);

	for (i = 0; i < NUM_RAPL_DOMAINS; i++)
	{

		sprintf(filename, "/sys/bus/event_source/devices/power/events/%s",
				rapl_domain_names[i]);

		fff = fopen(filename, "r");

		if (fff != NULL)
		{
			fscanf(fff, "event=%x", &config[i]);
			// printf("\tEvent=%s Config=%d ",rapl_domain_names[i],config[i]);
			fclose(fff);
		}
		else
		{
			continue;
		}

		sprintf(filename, "/sys/bus/event_source/devices/power/events/%s.scale",
				rapl_domain_names[i]);
		fff = fopen(filename, "r");

		if (fff != NULL)
		{
			fscanf(fff, "%lf", &scale[i]);
			// printf("scale=%g ",scale[i]);
			fclose(fff);
		}

		sprintf(filename, "/sys/bus/event_source/devices/power/events/%s.unit",
				rapl_domain_names[i]);
		fff = fopen(filename, "r");

		if (fff != NULL)
		{
			fscanf(fff, "%s", units[i]);
			// printf("units=%s ",units[i]);
			fclose(fff);
		}

		// printf("\n");
	}

	for (j = 0; j < total_packages; j++)
	{

		for (i = 0; i < NUM_RAPL_DOMAINS; i++)
		{

			fd_perf[i][j] = -1;

			memset(&attr, 0x0, sizeof(attr));
			attr.type = type;
			attr.config = config[i];
			if (config[i] == 0)
				continue;

			fd_perf[i][j] = perf_event_open(&attr, -1, package_map[j], -1, 0);
			if (fd_perf[i][j] < 0)
			{
				if (errno == EACCES)
				{
					paranoid_value = check_paranoid();
					if (paranoid_value > 0)
					{
						// printf("\t/proc/sys/kernel/perf_event_paranoid is %d\n",paranoid_value);
						// printf("\tThe value must be 0 or lower to read system-wide RAPL values\n");
					}

					// printf("\tPermission denied; run as root or adjust paranoid value\n\n");
					return -1;
				}
				else
				{
					// printf("\terror opening core %d config %d: %s\n\n",
					//	package_map[j], config[i], strerror(errno));
					return -1;
				}
			}
		}
	}
	return 0;
}

double result_energy_rapl_perf[NUM_RAPL_DOMAINS];

static double *rapl_perf_after(int core)
{
	int i, j;

	for (i = 0; i < NUM_RAPL_DOMAINS; i++)
	{
		result_energy_rapl_perf[i] = 0;
	}

	for (j = 0; j < total_packages; j++)
	{
		// printf("\tPackage %d:\n",j);

		for (i = 0; i < NUM_RAPL_DOMAINS; i++)
		{

			if (fd_perf[i][j] != -1)
			{
				read(fd_perf[i][j], &value, 8);
				close(fd_perf[i][j]);

				// printf("\t\t%s Energy Consumed: %lf %s\n",
				// 	rapl_domain_names[i],
				// 	(double)value*scale[i],
				// 	units[i]);
				result_energy_rapl_perf[i] += value * scale[i];
			}
		}
	}
	return result_energy_rapl_perf;
}

long long before[MAX_PACKAGES][NUM_RAPL_DOMAINS];
long long after[MAX_PACKAGES][NUM_RAPL_DOMAINS];

static int rapl_sysfs_before(int core)
{

	char event_names[MAX_PACKAGES][NUM_RAPL_DOMAINS][256];
	char filenames[MAX_PACKAGES][NUM_RAPL_DOMAINS][256];
	char basename[MAX_PACKAGES][256];
	char tempfile[256];

	int valid[MAX_PACKAGES][NUM_RAPL_DOMAINS];
	int i, j;
	FILE *fff;

	// printf("\nTrying sysfs powercap interface to gather results\n\n");

	for (j = 0; j < total_packages; j++)
	{
		i = 0;
		sprintf(basename[j], "/sys/class/powercap/intel-rapl/intel-rapl:%d",
				j);
		sprintf(tempfile, "%s/name", basename[j]);
		fff = fopen(tempfile, "r");
		if (fff == NULL)
		{
			fprintf(stderr, "\tCould not open %s\n", tempfile);
			return -1;
		}
		fscanf(fff, "%s", event_names[j][i]);
		valid[j][i] = 1;
		fclose(fff);
		sprintf(filenames[j][i], "%s/energy_uj", basename[j]);

		/* Handle subdomains */
		for (i = 1; i < NUM_RAPL_DOMAINS; i++)
		{
			sprintf(tempfile, "%s/intel-rapl:%d:%d/name",
					basename[j], j, i - 1);
			fff = fopen(tempfile, "r");
			if (fff == NULL)
			{
				// fprintf(stderr,"\tCould not open %s\n",tempfile);
				valid[j][i] = 0;
				continue;
			}
			valid[j][i] = 1;
			fscanf(fff, "%s", event_names[j][i]);
			fclose(fff);
			sprintf(filenames[j][i], "%s/intel-rapl:%d:%d/energy_uj",
					basename[j], j, i - 1);
		}
	}

	/* Gather before values */
	for (j = 0; j < total_packages; j++)
	{
		for (i = 0; i < NUM_RAPL_DOMAINS; i++)
		{
			if (valid[j][i])
			{
				fff = fopen(filenames[j][i], "r");
				if (fff == NULL)
				{
					fprintf(stderr, "\tError opening %s!\n", filenames[j][i]);
				}
				else
				{
					fscanf(fff, "%lld", &before[j][i]);
					fclose(fff);
				}
			}
		}
	}

	// for(j=0;j<total_packages;j++) {
	// 	printf("\tPackage %d\n",j);
	// 	for(i=0;i<NUM_RAPL_DOMAINS;i++) {
	// 		if (valid[j][i]) {
	// 			printf("before \t\t%s\t: %lfJ\n",event_names[j][i],
	// 				((double)before[j][i])/1000000.0);
	// 		}
	// 	}
	// }

	return 0;
}

double result_energy[];
char *name_events[4];

static double *rapl_sysfs_after(int core)
{
	int size_vector = total_packages * NUM_RAPL_DOMAINS;
	result_energy[size_vector];
	// name_events[4];
	char event_names[MAX_PACKAGES][NUM_RAPL_DOMAINS][256];
	char filenames[MAX_PACKAGES][NUM_RAPL_DOMAINS][256];
	char basename[MAX_PACKAGES][256];
	char tempfile[256];
	int valid[MAX_PACKAGES][NUM_RAPL_DOMAINS];
	int i, j;
	FILE *fff;

	// printf("\nTrying sysfs powercap interface to gather results\n\n");

	/* /sys/class/powercap/intel-rapl/intel-rapl:0/ */
	/* name has name */
	/* energy_uj has energy */
	/* subdirectories intel-rapl:0:0 intel-rapl:0:1 intel-rapl:0:2 */

	for (j = 0; j < total_packages; j++)
	{
		i = 0;
		sprintf(basename[j], "/sys/class/powercap/intel-rapl/intel-rapl:%d",
				j);
		sprintf(tempfile, "%s/name", basename[j]);
		fff = fopen(tempfile, "r");
		if (fff == NULL)
		{
			fprintf(stderr, "\tCould not open %s\n", tempfile);
			return 0;
		}
		fscanf(fff, "%s", event_names[j][i]);
		valid[j][i] = 1;
		fclose(fff);
		sprintf(filenames[j][i], "%s/energy_uj", basename[j]);

		/* Handle subdomains */
		for (i = 1; i < NUM_RAPL_DOMAINS; i++)
		{
			sprintf(tempfile, "%s/intel-rapl:%d:%d/name",
					basename[j], j, i - 1);
			fff = fopen(tempfile, "r");
			if (fff == NULL)
			{
				// fprintf(stderr,"\tCould not open %s\n",tempfile);
				valid[j][i] = 0;
				continue;
			}
			valid[j][i] = 1;
			fscanf(fff, "%s", event_names[j][i]);
			fclose(fff);
			sprintf(filenames[j][i], "%s/intel-rapl:%d:%d/energy_uj",
					basename[j], j, i - 1);
		}
	}

	/* Gather after values */
	for (j = 0; j < total_packages; j++)
	{
		for (i = 0; i < NUM_RAPL_DOMAINS; i++)
		{
			if (valid[j][i])
			{
				fff = fopen(filenames[j][i], "r");
				if (fff == NULL)
				{
					fprintf(stderr, "\tError opening %s!\n", filenames[j][i]);
				}
				else
				{
					fscanf(fff, "%lld", &after[j][i]);
					fclose(fff);
				}
			}
		}
	}

	// for(j=0;j<total_packages;j++) {
	// 	printf("\tPackage %d\n",j);
	// 	for(i=0;i<NUM_RAPL_DOMAINS;i++) {
	// 		if (valid[j][i]) {
	// 			printf("after \t\t%s\t: %lfJ\n",event_names[j][i],((double)after[j][i])/1000000.0);
	// 		}
	// 	}
	// }

	for (i = 0; i < NUM_RAPL_DOMAINS; ++i)
	{
		result_energy[i] = 0;
	}

	// double temp;
	for (j = 0; j < total_packages; j++)
	{
		for (i = 0; i < NUM_RAPL_DOMAINS; i++)
		{
			if (valid[j][i])
			{
				// temp = (((double)after[j][i]-(double)before[j][i])/1000000.0);
				result_energy[i] += (((double)after[j][i] - (double)before[j][i]) / 1000000.0);
				name_events[i] = event_names[j][i];
			}
		}
	}

	return result_energy;
}

//***********************MAIN**********************************************
int main(int argc, char **argv)
{
	FILE *fp;

	double *res_energy_sysfs;
	double *res_energy_rapl_perf;

	char *s;

	int c;
	int force_msr = 1;
	int force_perf = 1;
	int force_sysfs = 0;
	int core = 0;
	int cpu_model;

	byte measurement_count = 3;

	(void)force_sysfs;

	while ((c = getopt(argc, argv, "c:hmps")) != -1)
	{
		switch (c)
		{
		case 'm':
			force_msr = 0;
			measurement_count -= 1;
			break;
		case 'p':
			force_perf = 0;
			measurement_count -= 1;
			break;
		}
	}

	cpu_model = detect_cpu();
	detect_packages();
	// a+ or w
	fp = fopen("result.txt", "a+");
	// fprintf(fp,"Computer information\n");
	fprintf(fp, "New measurement\ncpu_model\t%i\n", cpu_model);
	fclose(fp);
	sleep(3);

	if (cpu_model > -1)
	{
		int j, i = 0;
		byte buf[100];

		buf[0] = 1;
		write_cmd(buf, 1);

		while (read_cmd(buf) > 0)
		{
			// a+ or a
			fp = fopen("result.txt", "a+");
			int n = buf[0];

			if (n == 1) // before
			{
				rapl_sysfs_before(core);
				if (force_perf)
					rapl_perf_before(core);
				if (force_msr)
					rapl_msr_before(core, cpu_model);
				buf[0] = 2;
				write_cmd(buf, 1);
			}
			if (n == 2) // after
			{
				res_energy_sysfs = rapl_sysfs_after(core);
				if (force_perf)
					res_energy_rapl_perf = rapl_perf_after(core);
				if (force_msr)
					rapl_msr_after(core, cpu_model);
				fprintf(fp, "\n%d. measurement\t", ++i);
				fprintf(fp, "sysfs");
				if (force_perf)
					fprintf(fp, "\tperf_event");
				if (force_msr)
					fprintf(fp, "\tMSR");
				fprintf(fp, "\n");
				for (j = 0; j < 4; j++)
				{
					// fprintf(fp,"%s\t",name_events[j]);
					fprintf(fp, "%s\t", rapl_domain_names[j]);
					fprintf(fp, "%lf\t", res_energy_sysfs[j]);
					if (force_perf)
						fprintf(fp, "%lf\t", res_energy_rapl_perf[j]);
					if (force_msr)
						fprintf(fp, "%lf", msr_results[j]);
					fprintf(fp, "\n");
				}
				fprintf(fp, "\n");

				buf[0] = measurement_count;
				write_cmd(buf, 1);

				if (force_msr)
				{
					s = "msr";
					write_cmd(s, strlen(s));

					buf[0] = 4;
					write_cmd(buf, 1);

					for (j = 3; j >= 0; --j)
					{
						byte data[sizeof(double)];
						write_cmd(rapl_domain_names[j], strlen(rapl_domain_names[j]));
						memcpy(data, &msr_results[j], sizeof msr_results[j]);
						write_cmd(data, sizeof msr_results[j]);
					}
				}

				if (force_perf)
				{
					s = "perf_event";
					write_cmd(s, strlen(s));

					buf[0] = 4;
					write_cmd(buf, 1);

					for (j = 3; j >= 0; --j)
					{
						byte data[sizeof(double)];
						write_cmd(rapl_domain_names[j], strlen(rapl_domain_names[j]));
						memcpy(data, &res_energy_rapl_perf[j], sizeof res_energy_rapl_perf[j]);
						write_cmd(data, sizeof res_energy_rapl_perf[j]);
					}
				}

				s = "sysfs";
				write_cmd(s, strlen(s));

				buf[0] = 4;
				write_cmd(buf, 1);

				for (j = 3; j >= 0; --j)
				{
					byte data[sizeof(double)];
					write_cmd(rapl_domain_names[j], strlen(rapl_domain_names[j]));
					memcpy(data, &res_energy_sysfs[j], sizeof res_energy_sysfs[j]);
					write_cmd(data, sizeof res_energy_sysfs[j]);
				}

				buf[0] = 1;
				write_cmd(buf, 1);
			}
			fclose(fp);
		}
	}
	else
	{
		return -1;
	}

	return 0;
}