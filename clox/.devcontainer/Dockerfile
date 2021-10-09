FROM debian:bullseye-20210902
RUN apt-get update && apt-get install -y lsb-release wget software-properties-common gnupg cppcheck curl
RUN curl -fsSL https://deb.nodesource.com/setup_16.x | bash -
RUN apt-get update && apt-get install -y nodejs
RUN wget https://apt.llvm.org/llvm.sh
RUN chmod +x llvm.sh
RUN ./llvm.sh 13
RUN ln -s /usr/bin/clang-13 /usr/bin/clang
RUN curl https://raw.githubusercontent.com/ensconced/simp/main/dist/main.js > /usr/local/bin/simp
RUN chmod +x /usr/local/bin/simp
