#!/bin/bash

##---------------------------------------------------##
##    Original Author: Raj Handique
##    Date Created: 12 September, 2024
##    Email: raj.handique519@deccansociety.org
##---------------------------------------------------##

#=======================================================#
# Bash Script to create customised FORTRAN program file #
#=======================================================#

# Read the filename
echo "Enter the program name: "
read FILE

FILENAME="$FILE".f90

# Check if filename already exists

if [ -e $FILENAME ];
then
    echo "Error creating file. ${FILENAME} already exists!"
    exit 1
fi

touch ${FILENAME}

CURRENT_DATE=$(date +"%d %B %Y")

cat <<EOF > "$FILENAME"
  !!------------------------------------!!
  !!  Original Author: Raj Handique     !!
  !!  PRN: 240210723                    !!
EOF

echo "  !!  Date Created: $CURRENT_DATE   !!" >> "$FILENAME"  

cat <<EOL >> "$FILENAME"
  !!  FORTRAN 95 - Source Code          !!
  !!------------------------------------!!

EOL

echo "  program $FILE"  >> "$FILENAME"

cat<<EOG >> "$FILENAME"
    !!---------------------------------------------------------
    !! This program will require the user to declare all the variables and typecast explicitly !!
    !!---------------------------------------------------------

    implicit none

    !!--------------------------------------------------------
    !! Declare the variables
    !!--------------------------------------------------------
    
    !!--------------------------------------------------------
    !! INTERFACE MESSAGE
    !!--------------------------------------------------------
    
    WRITE(*,*)'------------------------------------------------------------------'
    WRITE(*,*)'This is program written to '
    WRITE(*,*)'Original Author: Raj Handique'
EOG

echo "    WRITE(*,*)'Date Created: $CURRENT_DATE'" >> "$FILENAME"

cat <<EOD >> "$FILENAME"
    WRITE(*,*)'PRN: 24021073'
    WRITE(*,*)'------------------------------------------------------------------'

    
    !!--------------------------------------------------------
    !! Analysis Block
    !!--------------------------------------------------------
EOD

echo "  end program $FILE" >> "$FILENAME"


chmod +x "$FILENAME"

# Moving file to the custom directory

echo "Enter the path of the directory: "
read PATH_DIRECTORY

mkdir "$PATH_DIRECTORY"

mv "$FILENAME" "$PATH_DIRECTORY"

# Closing Log

echo "$FILENAME has been created successfully and moved to $PATH_DIRECTORY !"




