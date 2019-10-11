#!/usr/bin/env bash

set -e

if [[ $# == 0 ]]; then
  exec $0 amc amc-prove
fi

build () {
  stack build \
    --ghc-options "-optc-static -optl-static -fhide-source-paths" \
    --flag amuletml:amc-prove-server
}

build
rm -rfv result/
mkdir -p result/

for arg in $*; do
  cp ".stack-work/dist/x86_64-linux-tinfo6/Cabal-3.0.0.0/build/$arg/$arg" result/
done

if which upx &>/dev/null; then
  upx result/*
fi

version=$(grep version amuletml.cabal | head -1 | sed -re 's/version:\s*//g')
echo "Generating packages for amuletml $version"

# Generate an archive for the libraries:
tar -cvf result/amuletml-${version}-lib.tar.xz lib/

# Generate an Arch Linux package if 'makepkg' was found in the path
# Chaotic evil: we don't actually use makepkg

if ! which makepkg &>/dev/null; then
  exit 0
fi

echo "Generating Arch package…"

size="$(du result/ | sed -re 's/([0-9]+).*/\1/g')" # The way makepkg computes filesize is retarded so we ignore it
echo $size

sudo rm -rf pkg/
mkdir -p pkg/usr/{bin,lib/amuletml}
for arg in $*; do
  cp "result/$arg" pkg/usr/lib/amuletml/ -rv
done
cp lib/ pkg/usr/lib/amuletml/ -rv

cat >pkg/.PKGINFO <<EOF
pkgname = amuletml
pkgbase = amuletml
pkgver = $version-1
url = https://github.com/tmpim/amulet
builddate = $(date +'%s')
packager = $(git config --get user.name) <$(git config --get user.email)>
size = $size
license = BSD
arch = $(uname -m)
EOF

# Generate fake executables for the packages
for arg in $*; do

cat >"pkg/usr/bin/$arg" <<EOF
#!/usr/bin/env sh
cd /usr/lib/amuletml/
exec $arg "\$@"
EOF
chmod 755 "pkg/usr/bin/$arg"

done

sudo chown root:root -R pkg/*

touch pkg/.MTREE

list_package_files () {
  (
    export LC_COLLATE=C
    shopt -s dotglob globstar
    printf '%s\0' **/*
  )
}

pushd pkg &>/dev/null
list_package_files | LANG=C bsdtar -cf - --format=mtree \
  --options='!all,use-set,type,uid,gid,mode,time,size,md5,sha256,link' \
  --files-from - --exclude .MTREE | gzip -c -f -n > .MTREE

list_package_files | bsdtar --null --files-from - -cf ../result/amuletml-$version.pkg.tar.xz
popd &>/dev/null

echo "Generating generic binary distribution…"
# Generate a generic bindist with an "install.sh" script

pushd pkg &>/dev/null

# Remove Arch noise
rm .MTREE .PKGINFO

# Generate the installation script

cat >install.sh <<EOF
#!/usr/bin/env bash

PREFIX=\${PREFIX:-/}

if [[ "\$(whoami)" != "root" ]]; then
  echo "Please run \$0 with sudo"
  exit 1
fi

inst () {
  if [[ -f "./\$1" ]]; then
    echo "installing \$1"
    mkdir -p \${PREFIX}/\$(dirname "\$1")
    cp ./\$1 \${PREFIX}/\$1
  fi
}

EOF

list_package_files | while IFS= read -r -d '' file; do
  # Don't install the install script
  if ! echo $file | grep sh &>/dev/null; then
    echo "inst $file" >> install.sh
  fi
done

# Patch the executable scripts to use the prefix

for arg in $*; do

  echo "echo 'Patching $arg..'" >> install.sh
  echo "sed -ire \"s;cd /usr;cd \${PREFIX}/usr;g\" \${PREFIX}/usr/bin/$arg" >> install.sh

done

echo "echo \"Installed amuletml $version to \${PREFIX}\"" >> install.sh
chmod 755 install.sh

list_package_files | bsdtar --null --files-from - -cf ../result/amuletml-$version-bindist.tar.xz

popd &>/dev/null

echo "Cleaning up package files"
sudo rm -rf pkg/
