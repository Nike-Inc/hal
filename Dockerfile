# Start from the Lambda Env mirror
FROM lambci/lambda:provided

# Switch to root, so we can install packages and configure users
USER root

# Install packages required for building GHC
RUN yum -y install gcc gmp-devel zlib-devel

# Give users access to /tmp, as stack uses it to build GHC
RUN chmod a=rwx,o+t /tmp

# Custom stack images require a stack group and stack user in the stack group
RUN /usr/sbin/groupadd stack
RUN /usr/sbin/useradd stack -g stack

# Switch to the stack user so we can setup its home directory
USER stack
RUN mkdir ~/.stack
ENV PATH=/usr/sbin:$HOME/.local/bin:$PATH
RUN mkdir -p ~/.local/bin

# Install stack
RUN curl -L https://get.haskellstack.org/stable/linux-x86_64.tar.gz | tar xz --wildcards --strip-components=1 -C ~/.local/bin '*/stack'

# Switch back to the root user
USER root

# Disable the default `lambdci/lambda` entrypoint
ENTRYPOINT []
