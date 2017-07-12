{application, 'ecoap_bench', [
	{description, "Simple CoAP Server Benchmark Tool"},
	{vsn, "0.1.0"},
	{modules, ['bench','bench_worker','bench_worker_sup','ecoap_bench_app','ecoap_bench_server','ecoap_bench_sup']},
	{registered, [ecoap_bench_sup]},
	{applications, [kernel,stdlib,hdr_histogram,ecoap_common]},
	{mod, {ecoap_bench_app, []}},
	{env, []}
]}.