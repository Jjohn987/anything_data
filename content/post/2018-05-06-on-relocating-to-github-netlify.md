---
title: On Relocating to GitHub/Netlify
author: Jeremy Johnson
date: '2018-05-06'
slug: on-relocating-to-github-netlify
categories: [blogging]
tags: [blogdown, GitHub, netlify, hugo]
---

**Deep, labored breathing** 


Hello everyone, this is the opening post on my new blog, which I'm relocating from Wordpress to GitHub Pages and Netlify. It's so nice I've given it a name - because nice things have names!

But, why was I panting? The relocation effort wasn't easy. Why did I follow through with it? Becuase it is *worth the effort*. This post is evidence of my victory. Now please, let me explain.

A few months ago, when I explored options for creating a blog to help me keep track of my learning, and to display my progress (and creativity) to the world, I decided on Wordpress. It was convenient, quick to set up, and easy to forget about.

But Wordpress didn't: 

1. Display R code very well. 
2. Highlight or display code in code blocks. 
3. Provide **exposure**. 


The main purpose of a blog is to be seen, but my page at Wordpress was more of a hideout than a open facing space. I did some googling and found Blogdown. It is great, but in all honesty, migrating over was a **struggle**. I tried [this process](https://tclavelle.github.io/blog/blogdown_github/), but ultimately found [this You Tube tutorial](https://youtu.be/syWAKaj-4ck) the best. The general workflow was 

1. Create GitHub account and Repo 

2. Create new R Version Control Project (same name as repo I think) 

3. Use ```blogdown::new_site(theme = "")``` to create a new site in R, and edit the config.toml file to change up some of the blog paramaters to your liking.  

4. Use ```blogdown::build_site``` 

5. Commit/push to GitHub, then point to Netlify. (You'll need to edit settings for repo to be a GitHub Page, for more details refer to John Muschelli's tutorial on You Tube.) 

Now, it was all definitely **FRUSTRATING**. I had no experience with GitHub and found it very unintuitive. While wrangling my config.toml file(to change blog appearance), I experienced merge conflicts between my local files and my GitHub repo, and had no idea what to do. But I'm very happy with the end result. I know that I can easily avoid future frustrations simply by learning about GitHub. Secondly, if I ever want to switch a layout, well, switching Hugo Themes seems very easy. 

But, the best thing is that **you won't ever have to manually update a blog again** and you can simply stay in R, where the action is. 

Next -- I should probably migrate my old posts from the Wordpress grave. 






