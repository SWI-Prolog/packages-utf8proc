cmake_minimum_required(VERSION 2.8.12)
project(swipl-utf8proc)

include("../cmake/PrologPackage.cmake")
configure_file(config.h.cmake config.h)

swipl_plugin(
    unicode4pl
    C_SOURCES unicode4pl.c utf8proc.c
    C_LIBS ${LIBEDIT_LIBRARIES}
    PL_LIBS unicode.pl)
