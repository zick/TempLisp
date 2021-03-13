#!/bin/sh
while getopts e:hv: OPT; do
  case $OPT in
    "e") exp=$OPTARG;;
    "h") help=1;;
  esac
done
if [ -z "$exp" -a -z "$help" ]; then
  exp=`cat - | tr -d '\n'`
fi
if [ `which sbcl` ]; then
  lisp="sbcl --script templisp.lisp"
elif [ `which clisp` ]; then
  lisp="clisp templisp.lisp"
else
    echo "You need to install SBCL or CLISP"
fi
if [ -z "$exp" ]; then
  cat <<EOF
EOF
Usage:
  ./compile.sh -e "(car '(a b c))" > car.cc
    or
  ./compile.sh < fib5.lsp > fib5.cc
EOF
fi
echo $exp | sed -e 's/#+/_SP_/g' | sed -e 's/%/_PS_/g' | \
    sed -e 's/</_LT_/g' | sed -e 's/>/_GT_/g' | $lisp
