#!/usr/bin/env bash
set -eux

VERSION=$1
coursier fetch \
  org.scalameta:fastparse_2.11:$VERSION \
  org.scalameta:fastparse_2.12:$VERSION \
  org.scalameta:fastparse_2.13.0-RC1:$VERSION \
  org.scalameta:fastparse_2.13.0-RC1:$VERSION \
  org.scalameta:fastparse_native0.3_2.11:$VERSION \
  org.scalameta:fastparse_sjs0.6_2.11:$VERSION \
  org.scalameta:fastparse_sjs0.6_2.12:$VERSION \
  org.scalameta:fastparse_sjs0.6_2.13.0-RC1:$VERSION \
  org.scalameta:fastparse_sjs0.6_2.13.0-RC1:$VERSION \
  -r sonatype:public
