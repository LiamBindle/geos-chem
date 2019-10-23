FROM liambindle/penelope:0.1.0-ubuntu16.04-gcc7-netcdf4.5.0-netcdff4.4.4


# Make install location
RUN mkdir /opt/geos-chem && mkdir /opt/geos-chem/bin

# Make build directory
COPY . /gc-src
RUN cd /gc-src \
&&  mkdir build

SHELL ["/bin/bash", "-c"]

RUN echo "module load gcc/7" >> /init.rc \
&&  echo "spack load hdf5" >> /init.rc \
&&  echo "spack load netcdf" >> /init.rc \
&&  echo "spack load netcdf-fortran" >> /init.rc \
&&  echo "export PATH=$PATH:/opt/geos-chem/bin" >> /init.rc \
&&  echo "source /init.rc >> /etc/bash.bashrc"

# Build Standard
RUN cd /gc-src/build \
&&  cmake -DRUNDIR=IGNORE -DRUNDIR_SIM=standard .. \
&&  make -j install \
&&  cp geos /opt/geos-chem/bin/geos-chem-standard \
&& rm -rf /gc-src/build/*

# Build Tropchem
RUN cd /gc-src/build \
&&  cmake -DRUNDIR=IGNORE -DRUNDIR_SIM=tropchem .. \
&&  make -j install \
&&  cp geos /opt/geos-chem/bin/geos-chem-tropchem \
&& rm -rf /gc-src/build/*

# Build SOA_SVPOA
RUN cd /gc-src/build \
&&  cmake -DRUNDIR=IGNORE -DRUNDIR_SIM=complexSOA_SVPOA .. \
&&  make -j install \
&&  cp geos /opt/geos-chem/bin/geos-chem-soa_svpoa\
&& rm -rf /gc-src/build/*

RUN rm -rf /gc-src
