#!/usr/bin/env zsh

shell_config_file_path="$HOME/.zshrc"

install_deb() {
    install_exwm_dependencies() {
      # For transparent background and image
      sudo apt install -y feh picom

      # For desktop environment
      sudo apt install -y scrot brightnessctl playerctl

      # For tray apps
      sudo apt install -y blueman pasystray pavucontrol

      # For locking screen
      sudo apt install -y xss-lock suckless-tools

      # autorandr
      sudo apt install -y autorandr

      # dunst for notifycation
      sudo apt install -y dunst
    }

    install_exwm_dependencies
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
