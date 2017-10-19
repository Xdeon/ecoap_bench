PROJECT = ecoap_bench
PROJECT_DESCRIPTION = Simple CoAP Server Benchmark Tool
PROJECT_VERSION = 0.1.0

DEPS = hdr_histogram 
dep_hdr_histogram = git https://github.com/HdrHistogram/hdr_histogram_erl master

BUILD_DEPS = ecoap
dep_ecoap = git https://Xdeon@bitbucket.org/Xdeon/ecoap.git morepd

include erlang.mk

app:: rebar.config

# NO_AUTOPATCH = hdr_histogram

ERLC_OPTS += +report +verbose +warn_deprecated_function +warn_deprecated_type +warn_untyped_record +warn_unused_import

SHELL_OPTS = +K true
