
FROM kubespark/spark-base:v2.2.0-kubernetes-0.5.0


RUN apk --no-cache --update-cache add \
	gcc \
	gfortran \
	python \
	python-dev \
	py-pip \
	build-base \
	wget \
	freetype-dev \
	libpng-dev \
	openblas-dev \
    jpeg-dev \
	&& apk add --no-cache gdal py-gdal gdal-dev --repository http://nl.alpinelinux.org/alpine/edge/testing \
  	&& pip install matplotlib \
	&& pip install scikit-image \
	&& pip install pandas \
	&& pip install pygdal 


COPY griffin /root/griffin
ENV PYTHONPATH=$PYTHONPATH:/root/griffin


CMD ["bash"]



