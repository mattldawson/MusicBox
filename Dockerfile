FROM fedora:27

RUN dnf -y update \
    && dnf -y install \
        gcc-gfortran \
        gcc-c++ \
        netcdf-fortran-devel \
        cmake \
        wget \
        python \
        git \
        nodejs \
    && dnf clean all

# Copy the MusicBox code
COPY . /MusicBox/

# python modules needed in scripts
RUN pip3 install requests

# nodejs modules needed Mechanism-To-Code
RUN npm install express helmet

# clone the Mechanism-To-Code tool
RUN git clone https://github.com/NCAR/MechanismToCode.git

RUN cd MusicBox \
    && manage_externals/checkout_externals

# update the URLs for MechanismToCode
RUN sed -i 's/acom-conleyimac2.acom.ucar.edu/localhost/' Musicbox/Mechanism_collection/get_tag.py \
      && sed -i 's/acom-conleyimac2.acom.ucar.edu/localhost/' MusicBox/Mechanism_collection/preprocess_tag.py

# Command line arguments
ARG TAG_ID=false
ARG ENV_FTP_URL=false

# Get a tag and build the model
RUN if [ "$TAG_ID" = "false" ] ; then \
      echo "No mechanism specified" ; else \
      echo "Grabbing mechanism $TAG_ID" \
      && cd MechanismToCode \
      && nohup bash -c "node combined.js &" && sleep 4 \
      && cd ../MusicBox/Mechanism_collection \
      && python3 get_tag.py -tag_id $TAG_ID \
      && python3 preprocess_tag.py -source_dir configured_tags/$TAG_ID \
      && cd ../MusicBox_host \
      && mkdir build \
      && cd build \
      && cmake3 ../ -S ../src -B . \
      && make \
      ; fi

# Grab environmental data
RUN if [ "$ENV_FTP_URL" = "false" ] ; then \
      echo "No environmental data URL specified" ; else \
      echo "Grabbing environmental data at $ENV_FTP_URL" \
      && cd MusicBox_host/data \
      && wget "$ENV_FTP_URL" \
      && mv *.nc env_conditions.nc \
      ; fi
