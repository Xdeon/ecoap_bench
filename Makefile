PROJECT = ecoap_bench
PROJECT_DESCRIPTION = Simple CoAP Server Benchmark Tool
PROJECT_VERSION = 0.1.0

DEPS += hdr_histogram
dep_hdr_histogram = git https://github.com/HdrHistogram/hdr_histogram_erl master

DEPS += ecoap_common
dep_ecoap_common = git https://Xdeon@bitbucket.org/Xdeon/ecoap_common.git master

# NO_AUTOPATCH += hdr_histogram

include erlang.mk

app:: rebar.config
