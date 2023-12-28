Steps to running build.sh in a Centos 7 Docker image:

## 1. Build your customized image

    docker build  . --build-arg USER=$(id -un) --build-arg UID=$(id -u)  -t snowdrift-builder

This creates a user in the image with the same uid as your actual user, to avoid
permissions problems.

## 2. Run it

    docker run -it --mount "type=bind,source=/path/to/snowdrift-repo,destination=/home/$(id -un)/snowdrift" -p 3000:3000 --name snowbuild snowdrift-builder

Replace `/path/to/snowdrift-repo` with the path to your code repository. E.g. I run

    docker run -it --mount "type=bind,source=/home/b/Projects/snowdrift/repo-code,destination=/home/b/snowdrift" -p 3000:3000 --name snowbuild snowdrift-builder

## 3. Use build.sh

    ./build.sh test

    <wait for it>

With the bind mount, the container will see changes to your source code, so you
should be able to rebuild just by rerunning ./build.sh.

I haven't tried using the dev site yet (e.g. ./build.sh), but that's what the
`-p` is for in the run argument. Try it!

If you have an old .stack-work sitting around, you might need to move it out of
the way before starting the container. I'm not sure.

## 4. Repeat the process

After the first run, the container is left sitting around, which is what we
want. To start it again, run

    docker start -ai snowbuild

If you ever want to start from scratch, you have to remember to remove the
container first.

    docker container rm snowbuild

And sometimes stop it.

    docker container stop snowbuild
