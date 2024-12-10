## Overview

Repository for working group exploration of Nisqually Freshwater creel and investigation into possible biase.

## Dev notes

For major changes / additions of new modules, CBE suggests we use the following protocol:

- Start a new branch. In the terminal, this can be done with 
`git checkout -b branchname`
Instead of "branchname", use a easily identifiable descriptor. Using slashes is perfectly fine,
so "adding/power/analysis" might be a good choice for additional work 
- As you work in that branch, continue to commit, push, and pull like normal. The first time you push your branch,
you'll be given instruction by git on how to add your new branch to Github.
- Ifyou need to check how code was written in the other branch, use `git checkout` to switch
between branches. (But make sure to switch back to the branch you're working in before 
making relevant changes to the code)
- When the work for this specific change is done, make sure to commit, push, and pull. Then go to
github, and click the "make pull request" button. Add description of the changes made in the appropriate text box.
- Someone else will look over the changes made, and may comment to request changes before merging. Once they're in 
agreement that your changes are ready to go, they will merge your changes into the main branch.
- at this point, your branch can be deleted from your local machine and on github. The person doing the merging
probably deleted the branch on github. You can delete your local version with
`git branch -d branchname` in the terminal
- The work is done! Congrats!


- CH 11/20/2024
   - moving net mark rate excel file to original_data
   - created "tidy_net_mark_rates.R" in scripts folder to process data
   - saved cleaned "tidy" data in long format to cleaned_data folder as "tidy_net_mark_rates.csv"

- CH 12/10/2024
   - saved "20 years Net Timing Nisqually Clipped Unclipped Chinook.xlsx" file to original_data folder
   - created "tidy_net_mark_rates_v2.R" in scripts folder to process data
   - saved processed data to cleaned_data folder as "tidy_20-years-net-mark-rates.csv"