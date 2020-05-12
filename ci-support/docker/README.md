# Docker Images for CI

Docker containers are minimal Linux-based "virtual operating systems". GitLab
CI, and large parts of the devops world, use Docker.

Docker containers are booted up from a Docker image. In GitLab CI, one specifies
the image to use in the main CI configuration file, .gitlab-ci.yml.

Our Docker images are created from Dockerfiles. The images we use in CI are
built from Dockerfiles found in this directory.

GitLab pulls images from Docker Hub. We upload our images there when we create
new ones. Speaking of which:

## Maintenance

The script ../build-docker-images.sh will loop over the three images and
(re)build them. The results of the build are recorded in docker/images.yml,
which defines variables picked up by .gitlab-ci.yml. They are also pushed to
Docker Hub, which requires you to be a member of the snowdriftcoop organization.
Talk to chreekat for membership.

The script is *not* idempotent. It pulls the latest base images
from DockerHub, which will change over time.

## Reference

* https://hub.docker.com/repository/docker/snowdriftcoop
