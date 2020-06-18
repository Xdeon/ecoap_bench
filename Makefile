PROJECT = ecoap_bench
PROJECT_DESCRIPTION = Simple CoAP Server Benchmark Tool
PROJECT_VERSION = 0.1.0

DEPS = hdr_histogram ecoap
dep_hdr_histogram = git https://github.com/HdrHistogram/hdr_histogram_erl master
dep_ecoap = git https://github.com/Xdeon/ecoap dev

# NO_AUTOPATCH += hdr_histogram

include erlang.mk

app:: rebar.config

ERLC_OPTS += +report +verbose +warn_deprecated_function +warn_deprecated_type +warn_untyped_record +warn_unused_import +inline_list_funcs

SHELL_OPTS = +K true +spp true
