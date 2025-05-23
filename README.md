# Dominican Republic Financial Data Analysis - Multiple Banks

This repository contains a collection of R scripts designed for comprehensive analysis of financial data from multiple banks in the Dominican Republic. The project focuses on retrieving, processing, and visualizing key financial indicators using data from the public API of the Superintendence of Banks of the Dominican Republic.

## Contents

- **API Data Retrieval:** Scripts to connect securely to the API, handle pagination, and download detailed financial data spanning multiple years.
- **Data Cleaning & Transformation:** Functions and procedures to preprocess the raw data, ensure proper data types, and organize it for analysis.
- **Exploratory Data Analysis (EDA):** A suite of R scripts that perform statistical summaries, calculate important metrics such as delinquency rates, and analyze portfolio compositions by type.
- **Visualizations:** Multiple ggplot2 charts to illustrate trends in credit portfolios, delinquency (morosity) by type and entity, risk classifications, and evolution over time.
- **Exporting Results:** Code to save processed data and outputs in CSV format for further reporting or documentation.

## Key Features

- Automated querying and pagination management of the public API.
- Flexible filtering by date range, entity type, and indicators.
- Deep insights into credit portfolio evolution, risk classification, and delinquency rates.
- Clear, publication-ready plots to support financial decision-making and reporting.

## Requirements

- R (version 4.0 or higher)
- R packages: httr, jsonlite, tidyverse, lubridate, scales, stringr, dplyr, ggplot2

## Usage

1. Clone the repository.
2. Insert your API key in the designated R script or use a `.env` file.
3. Modify parameters for data requests such as date range and entity type.
4. Run the scripts sequentially to download, process, analyze, and visualize the data.
