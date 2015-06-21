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