Eshell V11.2.2.10  (abort with ^G)
1> c("rapl_erlang/energy_consumption").
{ok,energy_consumption}
2> c("measurements/higher_order_functions/filter_map").
{ok,filter_map}
3> energy_consumption:measure("./rapl-read.out",{filter_map, [recursive], [10,30]},100,"log.txt").