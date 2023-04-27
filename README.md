# tax-burden-on-labor
The Tax Foundation’s publication [A Comparison of the Tax Burden on Labor in OECD Countries](https://taxfoundation.org/publications/comparison-tax-burden-labor-oecd/) shows the tax burden on wages in  OECD countries. It covers personal income taxes and social security contributions paid by employees, social security contributions and payroll taxes paid by employers, and cash benefits received by workers.

## The Dataset

All the tax data used in this publication comes from the OECD's Taxing Wages Database.
The tax data used comes from Taxing wages - comparative tables (https://stats.oecd.org/index.aspx?DataSetCode=AWCOMP), Taxing Wages - Country tables, current model (https://stats.oecd.org/index.aspx?DataSetCode=AWCOU) and Taxing wages – Tax wedge decomposition (https://stats.oecd.org/index.aspx?DataSetCode=TXWDECOMP). The dataset includes information for each of the OECD countries on the income taxes paid by workers, their social security contributions, the family benefits they receive in the form of cash transfers as well as the social security contributions and payroll taxes paid by their employers.The amounts of taxes and social security contributions paid and cash benefits received are set out, programme by programme, for 8 different household types characterised by marital status, number of children, earnings levels expressed as proportion of average wages and whether there are one or two earners. The dataset covers the period 2000-2022.

"Taxing Wages" (http://oe.cd/taxing-wages) provides further information on the terminology used in Taxing Wages datasets together with a set of downloadable tables and charts.

The VAT rates and the VAT revenue ratio (VRR) used in this publication are from the OECD’s “Consumption Tax Trends 2022: VAT/GST and Excise, Core Design Features and Trends” taxation working papers.

## Explanation of Files in Repository

### /main directory

Location of the R code, the source documentation, and this README.

The R code reads in and downloads the necessary data, cleans the data, merges datasets, and produces intermediate and final output datasets and tables.

The source documentation cites all the sources used.

### /source-data

Location of **input** files to .R code file including:

- `iso_country_codes.csv` Dataset that includes all 279 sovereign states and dependent territories that have been assigned a country code by the International Organization for Standardization (ISO). Includes official country names, ISO-2 and ISO-3 country codes.

- `VAT_data.csv` Dataset that includes VAT rates and the VAT revenue ratio (VRR) from the OECD’s “Consumption Tax Trends 2022: VAT/GST and Excise, Core Design Features and Trends”.

### /final-outputs
Location of **output tables and figures ** that are included in the publication.

- `Figure1.csv` Table showing OECD  Average of the Tax Wedge of a Single Worker with no Children Earning a Nation's Average Wage, 2022.

- `Figure2.csv` Table comparing the Tax Burden in OECD Countries for a Single Worker with no Children Earning a Nation's Average Wage, 2022.

- `Figure3.csv` Table comparing the Tax Burden in OECD Countries for a Single Worker with no children and One-Earner Married with Two Childreen, 2022.

- `Figure4.csv` Table showing the OECD Average Tax Burden, 2000-2022. Tax Wedge of a Single Worker with no Children Earning a Nation's Average Wage.

- `Figure5.csv` Table showing the Most Notable Changes in the Tax Wedge between 2000 and 2022. Percentage-Point Difference between 2000 and 2022 in the Tax Wedge of a Single Worker with no Children Earning a Nation's Average Wage .

- `Figure6.csv` Table comparing the Tax Burden Accounting for VAT in OECD Countries. Tax Wedge of a Single Worker with no Children Earning a Nation's Average Wage, 2022.

- `Figure7.csv` [Not in 2023 version of report] Table showing the Economic Cost for the Marginal Dollar of Revenue Collected from Labor calculated as the Ratio of Marginal Tax Wedge to Average Tax Wedge, 2022

- `Table1.csv` Table comparing the Tax Wedge and it's decomposition of a Single Worker with no Children Earning a Nation's Average Wage, in the OECD Countries, 2022.

- `Table2.csv` Table comparing the Tax Wedge Including VAT of a Single Worker with no Children Earning a Nation's Average Wage, in the OECD Countries, 2022.

- `table_MTW.csv` Table with marginal tax wedges on labor and annual gross wages in US dollars, in the OECD Countries, 2022.
