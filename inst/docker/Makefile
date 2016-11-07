#
# Makefile
# fbreitwieser, 2016-10-20 11:54
#

PORT=5000

build:
	docker build -t florianbw/pavian .

run:
	docker run --rm -p $(PORT):80 florianbw/pavian

push:
	docker push florianbw/pavian

inspect:
	docker run --rm -i -t --entrypoint /bin/bash -p $(PORT):80 florianbw/pavian

# vim:ft=make
#
