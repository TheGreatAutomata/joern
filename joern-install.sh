#!/usr/bin/env sh
set -eu

if [ "$(uname)" = 'Darwin' ]; then
  # get script location
  # https://unix.stackexchange.com/a/96238
  if [ "${BASH_SOURCE:-x}" != 'x' ]; then
    this_script=$BASH_SOURCE
  elif [ "${ZSH_VERSION:-x}" != 'x' ]; then
    setopt function_argzero
    this_script=$0
  elif eval '[[ -n ${.sh.file} ]]' 2>/dev/null; then
    eval 'this_script=${.sh.file}'
  else
    echo 1>&2 "Unsupported shell. Please use bash, ksh93 or zsh."
    exit 2
  fi
  relative_directory=$(dirname "$this_script")
  SCRIPT_ABS_DIR=$(cd "$relative_directory" && pwd)
else
  SCRIPT_ABS_PATH=$(readlink -f "$0")
  SCRIPT_ABS_DIR=$(dirname "$SCRIPT_ABS_PATH")
fi

# Check required tools are installed.
check_installed() {
  if ! type "$1" > /dev/null; then
    echo "Please ensure you have $1 installed."
    exit 1
  fi
}

NON_INTERACTIVE=false
NO_DOWNLOAD=false

while test $# -gt 0
do
    case "$1" in
        --non-interactive)
          NON_INTERACTIVE=true
            ;;
        --no-download)
          NO_DOWNLOAD=true
            ;;
        --*) echo "bad option $1"
            ;;
        *) echo "argument $1"
            ;;
    esac
    shift
done

JOERN_DEFAULT_INSTALL_DIR=~/bin/joern
JOERN_DEFAULT_LINK_DIR="/usr/local/bin"
JOERN_DEFAULT_VERSION=""

if [ $NON_INTERACTIVE = true ]; then
  echo "non-interactive mode, using defaults"
  JOERN_INSTALL_DIR=$JOERN_DEFAULT_INSTALL_DIR
  JOERN_LINK_DIR=$JOERN_DEFAULT_LINK_DIR
  JOERN_VERSION=$JOERN_DEFAULT_VERSION
else
    # Confirm install with user.
    printf "This script will download and install the Joern tools on your machine. Proceed? [Y/n]: "
    read -r JOERN_PROMPT_ANSWER

    if [ "$JOERN_PROMPT_ANSWER" = "N" ] && [ "$JOERN_PROMPT_ANSWER" = "n" ]; then
	exit 0
    fi

    printf '%s' "Enter an install location or press enter for the default ($JOERN_DEFAULT_INSTALL_DIR): "
    read -r JOERN_INSTALL_DIR

    if [ "$JOERN_INSTALL_DIR" = "" ]; then
	JOERN_INSTALL_DIR=$JOERN_DEFAULT_INSTALL_DIR
    fi

    printf "Would you like to create a symlink to the Joern tools? [y/N]: "
    read -r JOERN_LINK_ANSWER

    if [ "$JOERN_LINK_ANSWER" = "Y" ] || [ "$JOERN_LINK_ANSWER" = "y" ]; then
	printf '%s' "Where would you like to link the Joern tools? (default $JOERN_DEFAULT_LINK_DIR): "
	read -r JOERN_LINK_DIR
	if [ "$JOERN_LINK_DIR" = "" ]; then
	    JOERN_LINK_DIR=$JOERN_DEFAULT_LINK_DIR
	fi
    fi

    printf "Please enter a Joern version/tag or press enter for the latest version: "
    read -r JOERN_VERSION

fi


mkdir -p $JOERN_INSTALL_DIR
# Download and extract the Joern CLI

check_installed "curl"

if [ $NO_DOWNLOAD ]; then
    sbt createDistribution
else
  if [ "$JOERN_VERSION" = "" ]; then
    curl -L "https://github.com/ShiftLeftSecurity/joern/releases/latest/download/joern-cli.zip" -o "$SCRIPT_ABS_DIR/joern-cli.zip"
  else
    curl -L "https://github.com/ShiftLeftSecurity/joern/releases/download/$JOERN_VERSION/joern-cli.zip" -o "$SCRIPT_ABS_DIR/joern-cli.zip"
  fi
fi


unzip -qo -d "$JOERN_INSTALL_DIR" "$SCRIPT_ABS_DIR"/joern-cli.zip
rm "$SCRIPT_ABS_DIR"/joern-cli.zip

# Link to JOERN_LINK_DIR if desired by the user
if [ -n "${JOERN_LINK_DIR+dummy}" ] && [ "$(whoami)" = "root" ]; then
  echo "Creating symlinks to Joern tools in $JOERN_LINK_DIR"
  ln -sf "$JOERN_INSTALL_DIR"/joern-cli/joern "$JOERN_LINK_DIR" || true
  ln -sf "$JOERN_INSTALL_DIR"/joern-cli/joern-cpg2scpg "$JOERN_LINK_DIR" || true
  ln -sf "$JOERN_INSTALL_DIR"/joern-cli/joern-parse "$JOERN_LINK_DIR" || true
fi

echo "Installing default queries"
CURDIR=$(pwd)
cd $JOERN_INSTALL_DIR/joern-cli
./joern-scan --updatedb
cd "$CURDIR"
echo "Install complete!"
