'''
Water Quality Data Format Converter
-----------------------------------
Author: A.J. Brown
Position: Agricultural Data Scientist
Email: Ansley.Brown@colostate.edu

Description:
This script provides functionalities to convert between new and old water 
quality data formats. It's designed as both a utility tool and an educational 
resource to showcase data conversion methodologies in Python.

TODO:
- New to old
    - Add functionality to convert new to old data format
- Old to new
    - add dictionaries to fill in location and trt with correct names
'''

# Import packages
import pandas as pd

# Import data
#old_data = pd.read_csv('Old Data/ardec2200_2021.csv', header=1)
old_data = pd.read_csv('Old Data/ardec2200_2022.csv', header=1)
new_data = pd.read_csv('Report/all_files_2023-12-13.csv')

old_cols = ['Location', 'Date', 'Irr/Storm', 'Station', 'Dup', 'event', 'ID', 'op',
'no2', 'no3', 'tkn', 'tp', 'selenium (mg/L)', 'TSS', 'E COLI', 'fecal',
'tss', 'Flow', 'OP', 'NO3', 'NH4', 'TKN', 'TP', 'E COLI.1', 'fecal.1',
'notes']
#clean_old_data(old_data).columns
'''
Should be:
['Location', 'Date', 'Irr/Storm', 'Station', 'Dup', 'event', 'ID', 'op',
'no2', 'no3', 'tkn', 'tp', 'selenium (mg/L)', 'TSS', 'E COLI', 'fecal',
'tss', 'Flow', 'OP', 'NO3', 'NH4', 'TKN', 'TP', 'E COLI.1', 'fecal.1',
'notes']
'''

new_cols = new_data.columns
'''  
Should be:  
['sample.id', 'lab.id.x', 'method', 'cas.number', 'analyte', 'result',
'units', 'dilution', 'result.reported.to', 'mdl', 'rl', 'report.basis',
'percent.moisture', 'percent.solid', 'non.detect', 'duplicate',
'location.name', 'treatment.name', 'method.name', 'event.type',
'event.count', 'lab.id.y', 'matrix', 'collected', 'received', 'hold',
'flag']
'''
# Data cleaning as necessary
def clean_old_data(old_df):
    """
    Cleans the old data format to prepare for conversion.
    inputs:
        old_df: pandas dataframe of old data format
    """
    # Strip trailing spaces from column names in old_data
    old_df.columns = old_df.columns.str.strip()

    # If there's a 'Rep' or 'DUP' column, rename it to 'Dup'.
    if 'DUP' in old_df.columns:
        old_df.rename(columns={'DUP': 'Dup'}, inplace=True)
    if 'Rep' in old_df.columns:
        old_df.rename(columns={'Rep': 'Dup'}, inplace=True)

    # make sure all columns exist as per the old_cols list, and if not, add them
    for col in old_cols:
        if col not in old_df.columns:
            old_df[col] = pd.NA
    
    # Convert values in 'Dup' column from "1" to "N" and "2" to "Y".
    old_df['Dup'] = old_df['Dup'].replace({"1": "N", "2": "Y"})

    # identify duplicates and fill into Dup column
        # sample ID's containing "-D" are duplicates
        # note that contains() auto assigns True/False values here
    old_df['Dup'] = old_df['ID'].str.contains('-D')

    # identify nan values and fill over total dataframe
        # if any cell contains NA, U, ND, nan, NAN, N/A, or is blank,
        # replace with nan
    old_df = old_df.replace(
        ['NA', 'U', 'ND', 'nan', 'NAN', 'N/A', ''], pd.NA)
    
    # correct MT, ST, CT, and IN to be MT1, ST1, CT1, and INF, respectively
    old_df['Station'] = old_df['Station'].replace(
        {'MT': 'MT1', 'ST': 'ST1', 'CT': 'CT1', 'IN': 'INF'})
    
    # correct E COLI to be e coli
    old_df['op'] = old_df['op'].replace({'E COLI': 'e coli'})

    return old_df

# Create dictionary to map equivalent columns and analyte names between formats
format_dictionary_updated = {
    'op': 'Phosphorus, Total Orthophosphate (as P)',
    'no2': 'Nitrogen, Nitrite  (As N)',
    'no3': 'Nitrogen, Nitrate (As N)',
    'tkn': 'Nitrogen, Total Kjeldahl',
    'tp': 'Phosphorus, Total (As P)',
    'selenium (mg/L)': 'Selenium, Total',
    'TSS': ['TSS', 'Suspended Solids (Residue, Non-Filterable)'],
    'Location': 'location.name',
    'Date': 'collected',
    'Irr/Storm': 'event.count',
    'Station': 'treatment.name',
    'Dup': 'duplicate',
    'ID': 'sample.id'
}

def convert_old_to_new(old_df, format_dict, output_csv=False):
    """
    Converts old data format to new data format.
    inputs:
        old_df: pandas dataframe of old data format
        format_dict: dictionary of new/old column names and analyte names
    returns:
        merged_df: pandas dataframe of new data format
    Example usage:
    >>> old_to_new_df = convert_old_to_new(old_data,
                                           format_dictionary_updated,
                                           output_csv=True)
    """
    # Clean the old data format
    old_df = clean_old_data(old_df)

    # Add method dataframe
    method_df = pd.DataFrame({
    'method': ['M4500 NH3 D - TKN_W 4500NH3 D', 'E300 - 300_W', 'E300 - 300_W', 
               'SM4500P E - P-Ortho M4500P E', 'SM4500P E - P_TW M4500P E', 
               'E160.1 - TDS_W_160.1', 'E120.1 - COND_W',
               'SW9040C - pH_W_9040C', 'SW9040C - pH_W_9040C', 
               'E160.2 - TSS_W_160.2', '200.8'],
    'cas.number': ['7727-37-9TKN', '14797-55-8', '14797-65-0', '7723-14-0', 
                   '7723-14-0', 'TDS', 'COND', 'PH', 'TEMP', 'TSS', 'SE'],
    'analyte': ['Nitrogen, Total Kjeldahl', 'Nitrogen, Nitrate (As N)',
                'Nitrogen, Nitrite  (As N)', 
                'Phosphorus, Total Orthophosphate (as P)', 
                'Phosphorus, Total (As P)', 
                'Total Dissolved Solids (Residue, Filterable)',
                'EC', 'pH', 'Temp Deg C @pH', 'TSS', 'Selenium, Total'],
    'mdl': [0.1, 0.15, 0.15, 0.01, 0.02, 5, 5, 0.1, 0, 2.5, 0.2],
    #note below: RL was not reported by ALS for selenium, just MDL
    'rl': [0.5, 0.5, 0.5, 0.05, 0.05, 10, 5, 0.1, 0, 2.5, 0.2] 
    })
    
    # Extract analytes from the format dictionary
    analyte_cols = [key for key in format_dict.keys() if key not in [
        'Location', 'Date', 'Irr/Storm', 'Station', 'Dup', 'ID']]
    
    # Melt the old dataframe to convert analyte columns to rows
    melted_data = old_df.melt(id_vars=[
        key for key in format_dict.keys() if key not in analyte_cols],
                              value_vars=analyte_cols,
                              var_name='analyte',
                              value_name='result')
    
    # Map the old analyte names to the new ones using format_dict
    # If the mapping is a list, default to the first value in the list
    melted_data['analyte'] = melted_data['analyte'].apply(
        lambda x: format_dict[x][0] if isinstance(
        format_dict[x], list) else format_dict[x])
    
    ## Add default values or calculate new values for non-intersecting columns

    # Add 'units' column with appropriate default values
    melted_data['units'] = "mg/L"
    melted_data.loc[
        melted_data['analyte'] == 'Selenium, Total', 'units'] = "ug/L"
    melted_data.loc[
        melted_data['analyte'] == 'EC', 'units'] = "umhos/cm"
    melted_data.loc[
        melted_data['analyte'] == 'pH', 'units'] = "pH"
    melted_data.loc[
        melted_data['analyte'] == 'Temp Deg C @pH', 'units'] = "Celcius"
    # add warning for selenium to double check units
    if 'Selenium, Total' in melted_data['analyte'].values:
        print(
            "Warning: Selenium detected. Please double-check the units for"
            " Selenium.")

    # Identify 0 values in the result column and set 'non.detect' column to True
    melted_data['non.detect'] = melted_data['result'] == 0

    # Merge with method_df to add method and cas.number columns
    merged_data = pd.merge(melted_data, method_df, on='analyte', how='left')
    
    # Rename columns based on format_dict for consistency
    merged_data.rename(columns=format_dict, inplace=True)

    # Ensure all columns from the new format are present
    for col in new_cols:
        if col not in merged_data.columns:
            merged_data[col] = pd.NA

    # Export to csv to Output folder if output_csv is True
    if output_csv:
        merged_data.to_csv('Data/old_to_new_data_py.csv', index=False)
    
    return merged_data

def convert_new_to_old(new_df, format_dict, output_csv=False):
    '''
    Converts new data format to old data format.
    inputs:
        new_df: pandas dataframe of new data format
        format_dict: dictionary mapping new column/analyte names to old names
        output_csv: boolean; if True, saves the converted df to a CSV
    returns:
        merged_df: pandas dataframe of old data format
    Example usage:
    >>> new_to_old_df = convert_new_to_old(new_data, 
                                           format_dictionary_updated,
                                           output_csv=True)
    '''
    
    # Pivot new data so each analyte becomes a column.
    pivoted_new_data = new_df.pivot(
        index='sample.id', columns='analyte', values='result'
    ).reset_index()
    
    # Merge original new data with pivoted data.
    merged_new_data = pd.merge(
        new_df.drop_duplicates(subset='sample.id'), 
        pivoted_new_data, on='sample.id', how='left'
    )
    
    # Create a reverse mapping from old to new format.
    reverse_format_dict = {}
    for key, value in format_dict.items():
        if isinstance(value, list):
            for item in value:
                reverse_format_dict[item] = key
        else:
            reverse_format_dict[value] = key
    
    # Rename columns to map from new format back to old format.
    merged_new_data.rename(columns=reverse_format_dict, inplace=True)
      
    # Ensure all columns from the old format are present. If missing, add them
    # and fill with NAs.
    for col in old_cols:
        if col not in merged_new_data.columns:
            merged_new_data[col] = pd.NA

    # Replace True/False values in the 'Dup' column.
    merged_new_data['Dup'] = merged_new_data['Dup'].map({True: 'Y', False: 'N'})
    
    # Retain the order of columns as in the old format.
    converted_data = merged_new_data[old_cols]
    
    # Export to csv if output_csv is True.
    if output_csv:
        converted_data.to_csv('Output/new_to_old_data_py.csv', index=False)

    return converted_data

############ Diagnostic code to check the conversion in Python only ############
def report_non_overlapping_columns(df1, df2):
    """
    Reports the columns that do not overlap during the conversion process.
    
    inputs:
        df1: pandas dataframe
        df2: pandas dataframe
    returns:
        print statements of column names unique to df1 and df2
    Example usage:
    >>> report_non_overlapping_columns(new_data, old_to_new_df)
    """
    # Get the column names of each dataframe
    columns_df1 = set(df1.columns)
    columns_df2 = set(df2.columns)

    # Find column names unique to df1
    unique_to_df1 = columns_df1 - columns_df2

    # Find column names unique to df2
    unique_to_df2 = columns_df2 - columns_df1

    # Find column names that intersect between df1 and df2
    intersecting_columns = columns_df1.intersection(columns_df2)

    print("Column names unique to df1:", unique_to_df1)
    print("Column names unique to df2:", unique_to_df2)
    print("Column names that intersect:", intersecting_columns)

# merge the 3 updated dataframes together found in the Report folder:
# import the 3 updated dataframes via read.csv
df_2021 = pd.read_csv('Report/2200_old_to_new_data_2021.csv')
df_2022 = pd.read_csv('Report/2200_old_to_new_data_2022.csv')
df_2023 = pd.read_csv('Report/all_files_2023-12-13.csv')

# Converting column names to lowercase and removing any whitespace
df_2021.columns = df_2021.columns.str.lower().str.replace(' ', '')
df_2022.columns = df_2022.columns.str.lower().str.replace(' ', '')
df_2023.columns = df_2023.columns.str.lower().str.replace(' ', '')

# Removing duplicate columns from the 2021 and 2022 DataFrames
df_2021_unique = df_2021.loc[:, ~df_2021.columns.duplicated()]
df_2022_unique = df_2022.loc[:, ~df_2022.columns.duplicated()]

# Concatenating the DataFrames
concatenated_unique_df = pd.concat([df_2021_unique, df_2022_unique, df_2023])

# drop rows where sample.id is NaN
concatenated_unique_df = concatenated_unique_df.dropna(subset=['sample.id'])

# change any anlyte TSS values to suspended solids
concatenated_unique_df['analyte'] = concatenated_unique_df['analyte'].replace(
    {'TSS': 'Suspended Solids (Residue, Non-Filterable)'}
)

# change Irr 1, Irr 2 to Irrigation 1, Irrigation 2 and so forth in the 
# event.count column
concatenated_unique_df['event.count'] = concatenated_unique_df['event.count'].replace(
    {'Irr 1': 'Irrigation 1', 'Irr 2': 'Irrigation 2', 'Irr 3': 'Irrigation 3',
     'Irr 4': 'Irrigation 4', 'Irr 5': 'Irrigation 5', 'Irr 6': 'Irrigation 6',
     'Irr 7': 'Irrigation 7', 'Irr 8': 'Irrigation 8', 'Irr 9': 'Irrigation 9',}
)

# export the merged dataframe to a csv
concatenated_unique_df.to_csv('Report/merged_data.csv', index=False)