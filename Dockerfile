FROM fukamachi/roswell:0.0.2.34
MAINTAINER Eitaro Fukamachi <e.arrows@gmail.com>

RUN mkdir -p ~/common-lisp
RUN git clone --depth 1 https://github.com/quickdocs/quickdocs-parser ~/common-lisp/quickdocs-parser
RUN git clone --depth 1 https://github.com/quickdocs/quickdocs-extracter ~/common-lisp/quickdocs-extracter