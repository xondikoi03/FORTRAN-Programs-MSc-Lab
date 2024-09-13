# FORTRAN Programs - Computational Laboratory

This repository contains all FORTRAN programs that are taught in the **Computational and Numerical Methods (PHY 506 MJP)** course in MSc-I Physics at Savitribai Phule Pune University. The programs will be updated on a weekly basis.

## How to compile and run the program:

1. To compile the program, run the following command on your terminal:

```
gfortran -o output_filename program.f90
```

2. To run the program, run the following command on your terminal:

```
./output_filename
```

## How to run the BASH script:

1. Run the BASH script using:

```
./f90script.sh
```

2. It will prompt you `Enter the program name:`. Enter the name you want your program to be named.

3. It will prompt you `Enter the name of the author: `. Enter your name.

4. It will prompt you `Enter any additional comments to be added in the beginning: `. Enter any comments you want to add.

5. It will prompt you `"Do you wish to declare any function sub-programs?(y/n)`. You need to answer y/N depending upon if your program requires a function sub-program.

6. When answered y or Y, it will prompt you `How many function sub-programs do you wish to declare?`. You need to enter the number of sub-programs you want to declare in your program.

7. After that it will prompt you `Name of the function sub program? n`, where n is the n-th sub-program. Enter the name of the sub-program. It will continue to prompt you for n-times.

8. After that it will prompt you `Enter the path of the directory:`. Enter the name of the directory you want to save the file in. 

That's it.

> **Note :** I'll update at the end of the semester, if I pass this course (FR!)
