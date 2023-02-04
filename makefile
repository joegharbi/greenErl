CC=gcc

SOURCES=rapl_erlang/rapl-read.c rapl_erlang/erlang_comm.c
EXEC_FILE=rapl-read.out
FLAGS=-O2 -Wall -lm

$(EXEC_FILE): $(SOURCES)
	$(CC) -o $(EXEC_FILE) $(SOURCES) $(FLAGS)

clean: 
	rm -f result.txt
	rm -f result
	rm -f avg
	rm -f log.txt
	find . -name "*.beam" -type f -delete
	find . -name "*.dump" -type f -delete
	find . -name "*.out" -type f -delete
	find . -name "*.pyc" -type f -delete


setup:
	modprobe msr
	echo 0 > /proc/sys/kernel/perf_event_paranoid
