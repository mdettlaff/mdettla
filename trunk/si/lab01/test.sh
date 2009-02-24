#!/bin/bash

tmpfile="xyz.txt"

echo Tautologie:

echo -n "p => p;" > $tmpfile
cat $tmpfile; echo -n -e "\t"; ./zadanie1.sh < $tmpfile
echo -n "(p&q) <=> (q&p);" > $tmpfile
cat $tmpfile; echo -n -e "\t"; ./zadanie1.sh < $tmpfile
echo -n "((pVq)Vr) <=> (pV(qVr));" > $tmpfile
cat $tmpfile; echo -n -e "\t"; ./zadanie1.sh < $tmpfile
echo -n "p <=> p&p;" > $tmpfile
cat $tmpfile; echo -n -e "\t"; ./zadanie1.sh < $tmpfile
echo -n "(p&(qVr)) <=> ((p&q)V(p&r));" > $tmpfile
cat $tmpfile; echo -n -e "\t"; ./zadanie1.sh < $tmpfile
echo -n "~(p&q) <=> (~p V ~q);" > $tmpfile
cat $tmpfile; echo -n -e "\t"; ./zadanie1.sh < $tmpfile
echo -n "~p => (p=>q);" > $tmpfile
cat $tmpfile; echo -n -e "\t"; ./zadanie1.sh < $tmpfile
echo -n "~p => (p=>q);" > $tmpfile
cat $tmpfile; echo -n -e "\t"; ./zadanie1.sh < $tmpfile
echo -n "((p=>q)&(p=>~q)) => ~p;" > $tmpfile
cat $tmpfile; echo -n -e "\t"; ./zadanie1.sh < $tmpfile
echo -n "(p => (q=>r)) => ((p=>q) => (p=>r));" > $tmpfile
cat $tmpfile; echo -n -e "\t"; ./zadanie1.sh < $tmpfile

echo -e "\nNie-tautologie:"
echo -n "p=>q;" > $tmpfile
cat $tmpfile; echo -n -e "\t"; ./zadanie1.sh < $tmpfile
echo -n "(p=>q) <=> (p=>~r);" > $tmpfile
cat $tmpfile; echo -n -e "\t"; ./zadanie1.sh < $tmpfile
echo -n "(p & (q=>r)) & (p & (q=>~r)) <=> (~p V ~q);" > $tmpfile
cat $tmpfile; echo -n -e "\t"; ./zadanie1.sh < $tmpfile
echo -n "(p V (p=>q)) & (q & p);" > $tmpfile
cat $tmpfile; echo -n -e "\t"; ./zadanie1.sh < $tmpfile

rm $tmpfile
