#!/usr/pkg/bin/bash

# #!/bin/sh

subject="Job application by Greg Matheson";

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
    $display $addresspart

	" < letter_bushiban | mutt  -e "set copy=yes" -e "my_hdr To: $display $addresspart" -e "my_hdr From: Greg Matheson <drbean@freeshell.org>" -e "my_hdr Subject: Job application by Greg Matheson" -a ../html/P6270001.JPG -a ../html/passport.jpg -a ../html/diploma.jpg

	error=$?;
	echo -n $display $addresspart;
	if [[ $error == 0 ]]; then echo " OK"; else echo " NOK"; fi;
	sleep 35;
done
