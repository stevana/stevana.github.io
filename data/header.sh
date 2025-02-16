#!/usr/bin/env bash

title="${title:-title}"
escaped_title="$(echo $title | sed 's/ /\+/g')"
date="${date:-date}"
place="${place:-place}"
url="${url:-url}"
author="${author:-author}"

cat <<EOT
<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml" lang="en" xml:lang="en">
<head>
  <meta charset="utf-8" />
  <meta name="viewport" content="width=device-width, initial-scale=1.0, user-scalable=yes" />
  <meta name="author" content="$author" />
  <title>$title</title>
  <link rel="stylesheet" href="../style.css?modified=2025-02-16" />
  <link rel="alternate" type="application/rss+xml"
        title="RSS feed"
        href="../rss.xml" />
  <script src="../script.js"></script>
  <script id="MathJax-script" async src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"></script>
  <script data-goatcounter="https://stevana-github-io.goatcounter.com/count"
        async src="//gc.zgo.at/count.js"></script>
</head>
<body>
<header id="title-block-header">
  <nav id="nav">
    <span class="title"><a href="/">Stevan's notes...</a></span>
    <a href="../about.html">About / <span class="work-with-me">Work with me</span></a>
    <a href="../rss.xml">Feed <img height="10px" src="../rss.svg" /></a>
  </nav>
</header>
<noscript>
  <img src="https://stevana-github-io.goatcounter.com/count?t=$escaped_title"
       alt="goatcounter">
</noscript>
<hr />
<main>
<h1>$title</h1>
<div class="date">Presented on $date at <a href="$url">$place</a></div>
EOT
