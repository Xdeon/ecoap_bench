PROJECT = ecoap_bench
PROJECT_DESCRIPTION = Simple CoAP Server Benchmark Tool
PROJECT_VERSION = 0.1.0

DEPS += hdr_histogram_erl
dep_hdr_histogram_erl = git https://github.com/HdrHistogram/hdr_histogram_erl master

DEPS += edown
dep_edown = git https://github.com/uwiger/edown.git master

include erlang.mk

app:: rebar.config
