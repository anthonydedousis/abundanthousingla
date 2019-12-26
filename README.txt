Datasets:
   ACS - American Community Survey neighborhood-level data on population, number of households, race, rent/income ratio, income, unemployed, rent-burdened (more info here: https://www.census.gov/data/developers/data-sets/acs-5year.html)

   Housing Supply by Neighborhood - number of multifamily and single-family homes by neighborhood in LA - original source is the LA County Assessor's Office (http://maps.assessor.lacounty.gov/GVH_2_2/Index.html?configBase=http://maps.assessor.lacounty.gov/Geocortex/Essentials/REST/sites/PAIS/viewers/PAIS_hv/virtualdirectory/Resources/Config/Default).  I'd take the multifamily number with a BIG grain of salt - filtering the original dataset was hard and I'm pretty sure my teammate excluded apartments in mixed-use buildings

   Zillow - ZRI (Zillow Rent Index) estimating monthly inflation-adjusted rents by neighborhood (https://www.zillow.com/research/data/)



Scripts:
	1. abundanthousingla/Scripts/HousingSupply_Merge_v2.R
		- The purpose of this script is to merge ACS, Zillow, and Housing Supply data for Los Angeles.  See the parent directory\'s README for more information.
		- First version of script was for a local copy of the data and testing.w
		- Input:
			- The 3 datasets above.
		- Output
			- SupplyZillowACS_merged.csv

