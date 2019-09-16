# Orange2-HW
Fall 1 Orange 2 HW repo

## Current assignments:

| Done?                    | Assignment               | Due Date |
|--------------------------|--------------------------|----------|
| :heavy_check_mark:       | Logistic Regression HW 1 |          |
| :heavy_check_mark:       | Linear Algebra HW 1      |          |
| :heavy_check_mark:       | Time Series HW 1         |          |
| :heavy_multiplication_x: | Logistic Regression HW 2 | 9/18     |
| :heavy_multiplication_x: | Logistic Regression HW 3 | 9/26     |
| :heavy_multiplication_x: | Time Series HW 2         | 9/25     |


# Basic Setup Instructions (Command line - Use Git Bash)
 - After being added as a collaborator, clone the repo where you want it
 ```
 git clone https://github.com/gclark422/Orange2-HW.git
 ```
 - Make any changes/add any files then do the following:
 ```
 git status
 ```
 - This will show useful output - for instance any files that you added, or changes you made to files (in red)
 - Then when you're ready to commit:
 ```
 git add --all
 git commit -m "add meaningful message here"
 git push origin master
 ```
 - This will push your changes to the master branch.
 - **IF this fails** then you first need to pull because it means you dont have the latest code from GitHub
 ```
 git pull origin master
 ```
 Then you can push :)
 
 # 2 useful command line commands
 `cd` - This means 'change directory'. Used as follows:
 ```
 cd [path or sub directory]
 ```
 Example:
 ```
 cd /users/Grant/Desktop
 ```
 This will navigate from my current position to desktop (assuming desktop is in a 'deeper' directory than the current)
 Another example to navigate 'up' or to the parent directory:
 ```
 cd ..
 ```
 ____
 `ls` - The 'list' command. This will list contents of a directory
 Example:
 ```
 ls -al
 ```
 This will show all files and subdirectories within the current working directory.
