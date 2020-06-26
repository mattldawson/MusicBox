FROM fedora:27

RUN dnf -y update \
    && dnf -y install \
        gcc-gfortran \
        gcc-c++ \
        netcdf-fortran-devel \
        cmake \
        wget \
        python \
        python3 \
        texlive-scheme-basic \
        'tex(type1cm.sty)' \
        'tex(type1ec.sty)' \
        git \
        nodejs \
        ncview \
    && dnf clean all

# Copy the MusicBox code
COPY . /MusicBox/

# python modules needed in scripts
RUN pip3 install requests numpy scipy matplotlib ipython jupyter pandas nose Django pillow

# clone and install the Mechanism-To-Code preprocessor service
RUN git clone https://github.com/NCAR/MechanismToCode.git
RUN cd MechanismToCode \
    && npm install

# Collect the rest of the code based on CESM tools
RUN cd MusicBox \
    && manage_externals/checkout_externals

# Command line arguments
ARG TAG_ID=false

# Get a tag and build the model
RUN if [ "$TAG_ID" = "false" ] ; then \
      echo "No mechanism specified" ; else \
      echo "Grabbing mechanism $TAG_ID" \
      && cd MechanismToCode \
      && nohup bash -c "node combined.js &" && sleep 4 \
      && cd ../MusicBox/Mechanism_collection \
      && python3 get_tag.py -tag_id $TAG_ID \
      && python3 preprocess_tag.py -mechanism_source_path configured_tags/$TAG_ID -preprocessor localhost:3000 \
      && python3 stage_tag.py -source_dir_kinetics configured_tags/$TAG_ID \
      && cd ../MusicBox_host \
      && mkdir build \
      && cd build \
      && cmake -D NETCDF_LIBRARIES="/usr/lib64/libnetcdff.so;/usr/lib64/libnetcdf.so" \
               -D NETCDF_INCLUDES_F90="/usr/lib64/gfortran/modules" \
               -D NETCDF_INCLUDES="/usr/lib64/gfortran/modules" \
               ../src \
      && make \
      ; fi

# Prepare the MusicBox Interactive web server
RUN mv MusicBox/music_box_interactive .

EXPOSE 8000

CMD ["python3", "music_box_interactive/manage.py", "runserver", "0.0.0.0:8000" ]
