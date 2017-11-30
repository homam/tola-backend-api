FROM homam/assrock

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
