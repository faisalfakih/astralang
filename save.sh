# Prompt the user for a commit message
echo "Enter commit message:"
read commit_message

# Add all changes to the staging area
git add .

# Commit the changes
git commit -m "$commit_message"

# Push the changes to the current branch on the remote repository
current_branch=$(git rev-parse --abbrev-ref HEAD)
git push origin $current_branch