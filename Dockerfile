FROM fukamachi/roswell
MAINTAINER Eitaro Fukamachi <e.arrows@gmail.com>

RUN mkdir -p /root/common-lisp
COPY modules/quickdocs-parser /root/common-lisp/quickdocs-parser
COPY . /root/common-lisp/quickdocs-extracter

# for asteroids & blackthorn-engine
RUN apt-get install -y libsdl1.2-dev

# for CFFI
RUN apt-get install -y libffi-dev

# for cells-gtk3
RUN apt-get install -y libgtkglext1-dev

# libuv for cl-libuv
RUN apt-get install -y libtool
RUN curl -SL https://github.com/libuv/libuv/archive/v1.6.1.tar.gz \
  | tar -xzC /tmp/ \
  && cd /tmp/libuv-1.6.1 \
  && sh autogen.sh \
  && ./configure \
  && make \
  && make install

# libSDL_gfx for asteroids
RUN apt-get install -y libsdl-gfx1.2-dev

# libSDL_image-1.2 for blackthorn-engine
RUN apt-get install -y libsdl-image1.2-dev

# libglut3 for cells-gtk3, cl-opengl
RUN apt-get install -y freeglut3-dev

# libhdf5 for cl-ana (hash-table-utils)
RUN apt-get install -y libhdf5-dev

# libgfortran.so.3 for cl-blapack
RUN apt-get install -y libgfortran3

# liblapack for cl-blapack
RUN apt-get install -y liblapack-dev

# libgdk-3.so for cl-cffi-gtk
RUN apt-get install -y libgtk-3-dev

# libmysqlclient_r.so for cl-mysql
RUN apt-get install -y libmysqlclient-dev

# libIL.so for cl-devil
RUN apt-get install -y libdevil-dev

# libenchant.so for cl-enchant
RUN apt-get install -y libenchant-dev

# libev.4 for cl-ev
RUN apt-get install -y libev-dev

# libfcgi for cl-fastcgi
RUN apt-get install -y libfcgi-dev

# libfbclient.so.2 for cl-fbclient
RUN apt-get install -y libfbclient2

# libsqlite3 for clsql
RUN apt-get install -y libsqlite3-dev

# libGeoIP for cl-geoip
RUN apt-get install -y libgeoip-dev

# libglfw2 for cl-glfw
RUN apt-get install -y libglfw2

# libglfw3 for cl-glfw3
RUN apt-get install -y libglfw-dev

# libkyotocabinet for cl-kyoto-cabinet
RUN apt-get install -y libkyotocabinet-dev

# libevent_core.so for cl-libevent2
RUN apt-get install -y libevent1-dev

# libpuzzle.so.1 for cl-libpuzzle
RUN apt-get install -y libpuzzle-dev

# libssh2.so.1 for cl-libssh2
RUN apt-get install -y libssh2-1-dev

# liblinear.so.1 for cl-libsvm
RUN apt-get install -y liblinear-dev

# libyaml-0.so.2.0.4 for cl-libyaml
RUN apt-get install -y libyaml-dev

# libLLVM-3.1 for cl-llvm
RUN apt-get install -y libllvm3.6

# libblas for lla
RUN apt-get install -y libblas-dev

# libsybdb.so for cl-mssql
RUN apt-get install -y libsybdb5

# libopenal.so for cl-openal
RUN apt-get install -y libopenal-dev

# libplplot for cl-plplot
RUN apt-get install -y libplplot-dev

# libportaudio.so.2 for cl-portaudio
RUN apt-get install -y libportaudio-dev

# libps.so.0 for cl-pslib
RUN apt-get install -y pslib1

# libRmath.so for cl-rmath
RUN apt-get install -y r-mathlib

# librrd.so.2 for cl-rrd
RUN apt-get install -y librrd-dev

# librsvg-2.so.2 for cl-rsvg2
RUN apt-get install -y librsvg2-dev

# libsane.so.1 for cl-sane
RUN apt-get install -y libsane-dev

# libSDL2-2.0.so.0 for cl-sdl2
RUN apt-get install -y libsdl2-dev

# libslp.so.1 for cl-slp
RUN apt-get install -y libslp-dev

# libsophia for cl-sophia
RUN mkdir -p /tmp/sophia-1.2.2
RUN curl -SL https://github.com/pmwkaa/sophia/archive/version_1.2.2.tar.gz \
  | tar -xzC /tmp/sophia-1.2.2 --strip-components=1 \
  && cd /tmp/sophia-1.2.2 \
  && make -f makefile dynamic \
  && mv libsophia.so /usr/lib

# libtcod for cl-tcod
RUN mkdir -p /tmp/libtcod-1.5.1
RUN curl -SL https://bitbucket.org/libtcod/libtcod/get/1.5.1.tar.gz \
  | tar -xzC /tmp/libtcod-1.5.1 --strip-components=1 \
  && cd /tmp/libtcod-1.5.1 \
  && make -f makefiles/makefile-linux64 release \
  && mv libtcod.so libtcodxx.so libtcodgui.so /usr/lib

# libtidy for cl-tidy
RUN apt-get install -y libtidy-dev

# libtokyocabinet for cl-tokyo-cabinet
RUN apt-get install -y libtokyocabinet-dev

# libfixposix for IOLib
RUN apt-get install -y libfixposix-dev

# libzmq for cl-zmq
RUN apt-get install -y libzmq-dev

# libassimp.so.3 for classimp
RUN apt-get install -y libassimp-dev

# libSDL_mixer-1.2 for pal
RUN apt-get install -y libsdl-mixer1.2-dev

# libfreeimage for clinch
RUN apt-get install -y libfreeimage-dev

# libhspell for hspell
RUN apt-get install -y hspell
