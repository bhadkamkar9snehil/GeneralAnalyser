# Contributing to GeneralAnalyser

Thank you for your interest in contributing to GeneralAnalyser! This document provides guidelines and instructions for contributing.

## Development Setup

1. Fork and clone the repository:
```R
git clone https://github.com/yourusername/GeneralAnalyser.git
cd GeneralAnalyser
```

2. Install dependencies:
```R
install.packages(c(
    "shiny",
    "shinydashboard",
    "shinydashboardPlus",
    "dplyr",
    "ggplot2",
    "plotly",
    "DT",
    "forecast",
    "caret",
    "cluster",
    "anomalize",
    "e1071",
    "rpart"
))
```

3. Load the package in development mode:
```R
devtools::load_all()
```

## Code Style

- Follow the [tidyverse style guide](https://style.tidyverse.org/)
- Use roxygen2 for documentation
- Keep functions focused and concise
- Add tests for new functionality

## Testing

Run tests using:
```R
devtools::test()
```

Write tests for:
- New functions
- Bug fixes
- Edge cases
- Error conditions

## Documentation

- Add roxygen2 documentation for all exported functions
- Include examples in function documentation
- Update vignettes when adding new features
- Keep README.md current

## Pull Request Process

1. Create a feature branch
2. Write tests
3. Update documentation
4. Run `devtools::check()`
5. Submit PR with clear description

## Code Review Guidelines

- Check code style
- Verify tests pass
- Ensure documentation is updated
- Look for potential performance issues
- Consider edge cases

## Package Structure

```
GeneralAnalyser/
├── R/
│   ├── analysis/      # Analysis modules
│   ├── data/         # Data handling
│   ├── ui/           # UI components
│   └── utils/        # Helper functions
├── tests/
│   └── testthat/     # Unit tests
├── vignettes/        # Package documentation
└── www/             # Web assets
```

## Module Guidelines

### Analysis Modules
- Keep analysis logic separate from UI
- Document assumptions
- Include error handling
- Add progress indicators

### UI Components
- Use consistent styling
- Follow accessibility guidelines
- Keep components modular
- Document dependencies

### Data Handlers
- Validate inputs
- Handle errors gracefully
- Document data requirements
- Include progress updates

## Performance Guidelines

- Profile code using `profvis`
- Optimize data operations
- Use reactive programming effectively
- Cache when appropriate

## Documentation Standards

### Function Documentation
```R
#' Function Title
#' 
#' @description
#' Detailed description
#' 
#' @param param1 Description of first parameter
#' @param param2 Description of second parameter
#' 
#' @return Description of return value
#' 
#' @examples
#' example_usage()
#' 
#' @export
```

### Code Comments
- Explain why, not what
- Document assumptions
- Note potential issues
- Reference related code

## Versioning

- Follow semantic versioning
- Document breaking changes
- Update DESCRIPTION file
- Add NEWS.md entries

## Issue Guidelines

When submitting issues:
1. Use issue templates
2. Include reproducible example
3. Specify R version
4. List package versions
5. Describe expected behavior

## Feature Requests

Include:
- Use case description
- Expected behavior
- Example workflow
- Potential implementation

## Common Tasks

### Adding a New Analysis Method
1. Create function in appropriate module
2. Add documentation
3. Write tests
4. Update UI
5. Add examples
6. Update vignettes

### Updating Documentation
1. Check roxygen2 comments
2. Update README.md
3. Review vignettes
4. Update NEWS.md
5. Check package site

### Release Process
1. Update version number
2. Run full test suite
3. Update documentation
4. Build package
5. Submit to CRAN

## Getting Help

- Open an issue
- Join discussions
- Read documentation
- Check vignettes

## Code of Conduct

- Be respectful
- Welcome contributions
- Help others learn
- Give constructive feedback

## License

By contributing, you agree to license your work under the same license as the project (MIT).