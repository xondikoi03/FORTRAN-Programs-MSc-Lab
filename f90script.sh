#!/bin/bash

#=====================================================#
# Bash script to make FORTRAN scripts with predefined #
# functions and comments.                             #
#=====================================================#

##---------------------------------------------------##
##    Original Author: Raj Handique                  ## 
##    Date Created: 13 September, 2024               ##
##    Email: raj.handique519@deccansociety.org       ##
##---------------------------------------------------##

# Prompt the user to enter the filename

echo "Enter the program name: "
read FILE

# Prompt the user about the author

echo "Enter the name of the author: "
read AUTHOR

# Prompt user for any additional comments
echo "Enter any additional comments to be added in the beginning: "
read INITIAL_COMMENT

FILENAME="$FILE".f90

# Check if filename already exists

if [ -e $FILENAME ];
then
    echo "Error creating file. ${FILENAME} already exists!"
    exit 1
fi

# Prompt the user if any function subprogram are to be declared:

echo "Do you wish to declare any function sub-programs?(y/n)"
read ANS

if [[ "$ANS"=="y" || "$ANS"=="Y" ]];
then
    echo "How many function sub-programs do you wish to declare?"
    read SUB

    funcsubarr=()
    
    for ((i=1; i<=SUB; i++)); do

	echo "Name of the function sub program?" ${i}
	read SUBPR
	funcsubarr[i]=$SUBPR
    done

elif [[ "$ANS"=="n" || "$ANS"=="N" ]];
then
    echo "No Sub-programs are declared!"
    exit 2
else
    
    echo "Invalid Statement, No Sub-programs are declared!"
    exit 3
fi


touch ${FILENAME}

CURRENT_DATE=$(date +"%d %B %Y")

echo !!-----------------------------------------------------  > $FILENAME
echo !! ${INITIAL_COMMENT}                                   >> $FILENAME
echo !! Original Author: ${AUTHOR}                           >> $FILENAME
echo !! Date Created: ${CURRENT_DATE}                        >> $FILENAME
echo !! FORTRAN 95 Source Code                               >> $FILENAME
echo !!----------------------------------------------------- >> $FILENAME

echo program ${FILE} >> $FILENAME

cat <<EOF >> $FILENAME

!!---------------------------------
!! This program will require the user to declare all the variables and typecast explicitly !!
!!---------------------------------

implicit none

!!---------------------------------
!! Declare the variables
!!---------------------------------

EOF

for ((k=1; k<=SUB; k++)); do
    echo "real::"${funcsubarr[k]} >> $FILENAME
done

cat <<EOL >> $FILENAME

!!---------------------------------
!! Analysis Block
!!---------------------------------

EOL

echo program ${FILE} >> $FILENAME

cat <<EOL >> $FILENAME

!!---------------------------------
!! function sub-program
!!---------------------------------

EOL

# Writing function Sub-Programs

for ((j=1; j<=SUB; j++)); do

    printf "\n" >> $FILENAME
    echo "real function" ${funcsubarr[j]}"(x)" >> $FILENAME
    printf "\n" >> $FILENAME
    echo "implicit none" >> $FILENAME
    printf "\n" >> $FILENAME
    echo "end function" ${funesubarr[j]} >> $FILENAME
    printf "\n" >> $FILENAME
    
done


chmod +x "$FILENAME"

# Moving the file to the required directory

echo "Enter the path of the directory: "
read PATH_DIRECTORY

# Checking if the directory exists

if [ -e $PATH_DIRECTORY ];
then
    echo "Error making directory. ${PATH_DIRECTORY} directory already exists!"
    exit 4
fi

mkdir "$PATH_DIRECTORY"

mv "$FILENAME" "$PATH_DIRECTORY"

# Closing Log / Messages

echo "$FILENAME has been successfully created and moved to $PATH_DIRECTORY !"


