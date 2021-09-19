FROM debian:bullseye-20210902
RUN apt-get update && apt-get install -y lsb-release wget software-properties-common gnupg cppcheck
RUN wget https://apt.llvm.org/llvm.sh
RUN chmod +x llvm.sh
RUN ./llvm.sh 13
RUN ln -s /usr/bin/clang-13 /usr/bin/clang
