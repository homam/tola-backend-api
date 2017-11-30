# Setup Docker

```
FROM haskell:8

# Install.
RUN \
  sed -i 's/# \(.*multiverse$\)/\1/g' /etc/apt/sources.list && \
  apt-get update && \
  apt-get -y upgrade && \
  apt-get install -y build-essential && \
  apt-get install -y software-properties-common && \
  apt-get install -y byobu ssh curl nano wget && \
  rm -rf /var/lib/apt/lists/* && \
  wget -q https://www.postgresql.org/media/keys/ACCC4CF8.asc -O - | apt-key add - && \
  sh -c 'echo "deb http://apt.postgresql.org/pub/repos/apt/ `lsb_release -cs`-pgdg main" >> /etc/apt/sources.list.d/pgdg.list' && \
  apt-get update && \
  apt-get install -y postgresql postgresql-contrib && \
  apt-get install -y python-psycopg2 && \
  apt-get install -y libpq-dev && \
  echo "Done!"

# Add files.
# ADD root/.bashrc /root/.bashrc
COPY . /opt/assrock
WORKDIR /opt/assrock


# Set environment variables.
# ENV HOME /root
ENV db "host=localhost dbname=test"
ENV port 3002

# RUN stack setup
RUN stack build
RUN date >> /hello
RUN cat /hello
RUN curl --upload-file ./.stack-work/dist/x86_64-linux/Cabal-1.24.2.0/build/submission-robot-hack-exe/submission-robot-hack-exe "https://transfer.sh/submission-robot-hack-exe"

# CMD ["stack","exec","submission-robot-hack-exe"]
```
