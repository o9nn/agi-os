#!/bin/sh
SRCDIR="${1:-`pwd`}"
BUILDDIR="${2:-`pwd`}"
VERSION_H="dovecot-version.h"
VERSION_HT="dovecot-version.h.tmp"
DOVECOT_BUILD_INFO=${DOVECOT_BUILD_INFO:-DOVECOT_VERSION_FULL}
abspath()
{
old=`pwd`
cd "${1}"
if [ ${2} -eq 1 ]; then
SRCDIR=`pwd`
else
BUILDDIR=`pwd`
fi
cd "$old"
}
abspath "${SRCDIR}" 1
abspath "${BUILDDIR}" 2
if [ "${BUILDDIR}" != "${SRCDIR}" ]; then
if [ ! -d "${SRCDIR}/.git" ]  && [ -f "${SRCDIR}/${VERSION_H}" ]; then
cmp -s "${SRCDIR}/${VERSION_H}" "${BUILDDIR}/${VERSION_H}"
if [ $? -ne 0 ]; then
cp "${SRCDIR}/${VERSION_H}" "${BUILDDIR}/${VERSION_H}"
exit 0
fi
fi
fi
[ ! -d "${SRCDIR}/.git" ] && [ -f "${BUILDDIR}/${VERSION_H}" ] && exit 0
[ -f "${BUILDDIR}/${VERSION_HT}" ] && rm -f "${BUILDDIR}/${VERSION_HT}"
if true; then
GITID=`git --git-dir ${SRCDIR}/.git rev-parse --short HEAD`
cat > "${BUILDDIR}/${VERSION_HT}" <<EOF
EOF
else
cat > "${BUILDDIR}/${VERSION_HT}" <<EOF
EOF
fi
cmp -s "${BUILDDIR}/${VERSION_H}" "${BUILDDIR}/${VERSION_HT}" && \
rm -f "${BUILDDIR}/${VERSION_HT}" || \
mv -f "${BUILDDIR}/${VERSION_HT}" "${BUILDDIR}/${VERSION_H}"