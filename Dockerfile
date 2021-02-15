FROM lambci/lambda:provided
USER root
RUN yum -y install gcc gmp-devel zlib-devel
RUN chmod a=rwx,o+t /tmp
RUN /usr/sbin/groupadd stack
RUN /usr/sbin/useradd stack -g stack
USER stack
RUN mkdir ~/.stack
ENV PATH=/usr/sbin:$HOME/.local/bin:$PATH
RUN mkdir -p ~/.local/bin
RUN curl -L https://get.haskellstack.org/stable/linux-x86_64.tar.gz | tar xz --wildcards --strip-components=1 -C ~/.local/bin '*/stack'
USER root
ENTRYPOINT []
