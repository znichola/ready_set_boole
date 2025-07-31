FROM haskell:9

RUN apt-get update && \
    apt-get install -y openssh-server && \
    mkdir /var/run/sshd

# test unsetting the password to see if this works
RUN echo 'root:boole' | chpasswd

RUN sed -i 's/^#PermitRootLogin.*/PermitRootLogin yes/' /etc/ssh/sshd_config

WORKDIR /app

EXPOSE 22

CMD ["/usr/sbin/sshd", "-D"]
