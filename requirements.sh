#!/usr/bin/env bash

install_go_tool() {
  # gopls
  go install golang.org/x/tools/gopls@latest

  # REPL!
  go install github.com/x-motemen/gore/cmd/gore@latest

  # Autocompletion
  go install github.com/stamblerre/gocode@latest

  # Documentation
  go install golang.org/x/tools/cmd/godoc@latest

  # Add/Removed Necessary Imports
  go install golang.org/x/tools/cmd/goimports@latest

  # Type-Safe Renaming of Go identifiers
  go install golang.org/x/tools/cmd/gorename@latest

  # Asks questions about your Gocode
  go install golang.org/x/tools/cmd/guru@latest

  # Generate tests based off of the func you're on
  go install github.com/cweill/gotests/gotests@latest

  # Add `json` or `bson` to structs easily
  go install github.com/fatih/gomodifytags@latest

  # delve golang debug
  go install github.com/go-delve/delve/cmd/dlv@latest

  # mockgen
  go install go.uber.org/mock/mockgen@latest
}

shell_config_file_path="$HOME/.zshrc"

install_deb() {
    install_go_if_not_exists() {
      if ! [ -x "$(command -v go)" ]; then
        echo -n "Go is NOT installed."
        snap install go --classic

        if ! grep -qxF 'export GOPATH=${HOME}/go' $shell_config_file_path
        then
          echo 'export GOPATH=${HOME}/go' >> $shell_config_file_path
          echo 'export PATH=$PATH:$GOPATH/bin' >> $shell_config_file_path
          echo -n 'Reload environment'
          source $shell_config_file_path

          # Install go dependencies
          install_go_tool
        fi
      fi
    }

    install_pyright_if_not_exists() {
      if ! [ -x "$(command -v pyright)" ]; then
        sudo snap install pyright --classic
      fi
    }

    install_yaml_language_server_if_not_exists() {
      if ! [ -x "$(command -v yaml-language-server)" ]; then
        sudo snap install yaml-language-server
      fi
    }

    install_node_if_not_exists() {
      if ! [ -x "$(command -v node)" ]; then
        sudo snap install node --classic
      fi
    }

    install_font() {
      sudo apt-get install fonts-powerline
    }

    install_rust_if_not_exists() {
      if ! [ -x "$(command -v rustc)" ]; then
        curl --proto '=https' --tlsv1.2 https://sh.rustup.rs -sSf | sh
      fi
    }

    # For vterm
    sudo apt install -y gcc g++ libtool-bin cmake ripgrep

    install_go_if_not_exists

    install_pyright_if_not_exists

    install_yaml_language_server_if_not_exists

    install_node_if_not_exists

    install_font

    install_rust_if_not_exists
}

PKGTYPE=unknown
ID=`id -u`

if [ -f /etc/redhat-release ] ; then
  PKGTYPE=rpm
elif [ -f /etc/system-release ] ; then
  # If /etc/system-release is present, this is likely a distro that uses RPM.
  PKGTYPE=rpm
else
  if uname -sv | grep 'Darwin' > /dev/null; then
    PKGTYPE=pkg
  elif [ -f /usr/bin/zypper ] ; then
    PKGTYPE=sus
  else
    PKGTYPE=deb
  fi
fi

if [ "$ID" -ne 0 ]; then
  if ! hash sudo 2>/dev/null; then
    echo "This script must be executed as the 'root' user or with sudo"
    exit 1
  else
    echo "Switching to root user to update the package"
    sudo -E $0 $@
    exit 0
  fi
fi

case $PKGTYPE in
  deb)
    install_deb
    ;;
  sus)
    install_suse
    ;;
  rpm)
    install_rpm
    ;;
  *)
    install_pkg
esac
