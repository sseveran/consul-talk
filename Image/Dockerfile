FROM centos:7
ENV container docker
RUN (cd /lib/systemd/system/sysinit.target.wants/; for i in *; do [ $i == systemd-tmpfiles-setup.service ] || rm -f $i; done); \
rm -f /lib/systemd/system/multi-user.target.wants/*;\
rm -f /etc/systemd/system/*.wants/*;\
rm -f /lib/systemd/system/local-fs.target.wants/*; \
rm -f /lib/systemd/system/sockets.target.wants/*udev*; \
rm -f /lib/systemd/system/sockets.target.wants/*initctl*; \
rm -f /lib/systemd/system/basic.target.wants/*;\
rm -f /lib/systemd/system/anaconda.target.wants/*;
VOLUME [ "/sys/fs/cgroup" ]
CMD ["/usr/sbin/init"]
#RUN systemctl disable firewalld

RUN curl -sSL https://s3.amazonaws.com/download.fpcomplete.com/centos/7/fpco.repo | tee /etc/yum.repos.d/fpco.repo

RUN yum -y update && yum clean all && yum -y install unzip stack curl openssl-devel postgresql-devel

ADD https://releases.hashicorp.com/consul/0.9.0/consul_0.9.0_linux_amd64.zip /tmp/consul.zip
RUN cd /bin \
  && unzip /tmp/consul.zip \
  && chmod +x /bin/consul \
  && rm /tmp/consul.zip
ADD ./Consul-Agent /etc/consul.d/
EXPOSE 8300 8301 8301/udp 8302 8302/udp 8400 8500 8600 53/udp
ENV DNS_RESOLVES consul

ENV PATH /root/.local/bin/:$PATH

ENTRYPOINT ["/bin/consul", "agent", "-config-dir=/etc/consul.d/"]
