#!/usr/bin/env bash
sbt ++2.11.12 \
    utilsJS/publishSigned \
    utilsJVM/publishSigned \
    utilsNative/publishSigned \
    fastparseJS/publishSigned \
    fastparseJVM/publishSigned \
    fastparseNative/publishSigned
sbt ++2.12.8 \
    utilsJS/publishSigned \
    utilsJVM/publishSigned \
    fastparseJS/publishSigned \
    fastparseJVM/publishSigned
sbt ++2.13.0-RC1 \
    utilsJS/publishSigned \
    utilsJVM/publishSigned \
    fastparseJS/publishSigned \
    fastparseJVM/publishSigned
