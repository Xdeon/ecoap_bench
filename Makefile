PROJECT = ecoap_bench
PROJECT_DESCRIPTION = Simple CoAP Server Benchmark Tool
PROJECT_VERSION = 0.1.0

DEPS += hdr_histogram
dep_hdr_histogram = git https://github.com/HdrHistogram/hdr_histogram_erl master

DEPS += ecoap_common
dep_ecoap_common = git https://Xdeon@bitbucket.org/Xdeon/ecoap_common.git dev

# NO_AUTOPATCH = hdr_histogram

ERLC_OPTS += +report +verbose +warn_deprecated_function +warn_deprecated_type +warn_untyped_record +warn_unused_import

# SHELL_OPTS = +K true +sbt s

include erlang.mk

app:: rebar.config
