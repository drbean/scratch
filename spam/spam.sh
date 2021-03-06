#!/usr/pkg/bin/bash

# #!/bin/sh

subject="Job application by Greg Matheson";

# ./spam.sh < addresses

#for address in \
#	"Greg_Matheson <gregoer@healthy..chinmin.tw>" \
#	"Greg Matheson <greg@healthy.chinmin.edu.tw>" \
#	"Greg J Matheson <greg@healthy.chinmin.edu.tw>" \
#	"Greg John Matheson <greg@healthy.chinmin.edu.tw>" \
#	;
#do
#	n=0;
#	for part in $address;
#	do
#		addressarray[$((n++))]=$part;
#	done;

while read -a addressarray;
do
	last=${#addressarray[@]};
	addresspart=${addressarray[$((--last))]};
	unset addressarray[$last];
	display=${addressarray[*]};
	sed -e "1i\\
To: $display $addresspart
	" -e "1i\\
From: Greg Matheson <drbean@freeshell.org>
	" -e "1i\\
Subject: $subject
	" -e "1i\\
Content-Type: text/plain; charset=\"UTF-8\"
	" < letter_bushiban | /usr/sbin/sendmail -oem -oi $addresspart;

	error=$?;
	echo -n $display $addresspart;
	if [[ $error == 0 ]]; then echo " OK"; else echo " NOK"; fi;
	sleep 35;
done
