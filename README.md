## Prerequisites

### Install Docker

- [for Mac](https://docs.docker.com/docker-for-mac/install/)
- [for Linux](https://docs.docker.com/install/linux/docker-ce/ubuntu/)
- [for Windows](https://docs.docker.com/docker-for-windows/install/)

After following the instructions, remember running the following:
```
sudo groupadd docker
sudo usermod -aG docker $USER
```
You may need to logout/reboot before the changes come into effect.


### Install required extensions in VSCode:

- Docker: ms-azuretools.vscode-docker
- Remote - Containers : ms-vscode-remote.remote-containers
  - Documentation available [here](https://code.visualstudio.com/docs/remote/containers)


## Setting up `renv`

1. If you have a local cache of packages in your host machine, check in `devcontainer.json` that it is correctly mounted during the image build.
   - Currently, it is assumed that `renv`'s cache is located in the `/.local/share/renv/cache` directory under the `$HOME` folder.
2. The first time you run the container after building the image, open an `R` session and run `renv::init()` to initialize renv inside your project.


## Additional configurations

### Optional: Enable pre-commits

To enable pre-commits, run the shell script `setup_precommit.sh`.

### Optional: Install pandoc

If you plan to render rmarkdown files, `pandoc` must be externally installed. Just run the script `install_pandoc.sh` provided inside the `shell_scripts` folder.

## Known issues

### VS Code does not find `languageserver`

In order for the linter and formatter to work inside the container, VS Code needs to be able to locate the `languageserver` package (along with it's many dependencies), which may not happen immediately after running the container.

The easiest way to deal with this is to install `languageserver` after initializing `renv`. Once this is done, the package will be installed in your renv cache, and as a result will always be available in this and future projects.