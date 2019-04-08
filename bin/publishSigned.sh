#!/usr/bin/env bash
sbt ++2.11.12 \
    fastparseJS/publishSigned \
    fastparseJVM/publishSigned \
    fastparseNative/publishSigned
sbt ++2.12.8 \
    fastparseJS/publishSigned \
    fastparseJVM/publishSigned
sbt ++2.13.0-RC1 \
    fastparseJS/publishSigned \
    fastparseJVM/publishSigned
