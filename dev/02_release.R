
message("which version element to bump?")
desc::desc_bump_version(
  which = readline(": ")
)
git2r::add(path = "DESCRIPTION")
git2r::commit(message = paste0("build: v", desc::desc_get_version()))

git2r::tag(name = paste0("v", desc::desc_get_version()))
system2("git", "push")
system2("git", c("push", "--tags"))
