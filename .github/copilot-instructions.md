# Copilot Code Review Instructions

This file provides guidance for GitHub Copilot when reviewing code in the d3heatmap R package repository.

## Repository Overview

This is an R package that implements interactive heatmaps using htmlwidgets and D3.js. The package provides:
- Interactive heatmap visualizations usable from R console, RStudio viewer, R Markdown, and Shiny apps
- Clustering and dendrograms functionality
- A modern API leveraging magrittr piping

## Code Review Guidelines

When reviewing code changes in this repository, please consider:

### R Code Standards
- Follow R package development best practices
- Ensure proper documentation using roxygen2 comments
- Check for proper NAMESPACE exports
- Verify function parameters are properly typed and documented
- Look for potential memory or performance issues with large datasets

### htmlwidgets / JavaScript Integration
- Verify proper data binding between R and JavaScript
- Check for potential XSS vulnerabilities in user-provided content
- Ensure D3.js usage follows best practices
- Validate JSON serialization of R objects

### Shiny Integration
- Review reactive expressions for efficiency
- Check for proper input validation
- Ensure outputs are properly rendered

### Testing and Documentation
- Encourage test coverage for new functionality
- Verify examples in documentation are accurate
- Check that NEWS.md is updated for user-facing changes

### Compatibility
- Consider backward compatibility with existing API
- Check for R version compatibility issues
- Verify htmlwidgets compatibility
