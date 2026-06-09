import re
from copy import deepcopy
import datetime as dt
import pandas as pd


def main():
    '''
    Identify which transactions made by each static panelist occur while
    the panelist belongs to the static panel
    '''

    # Use combined data (original + March 2022)?
    use_combo = True

    if use_combo:
        inpath_summary = 'data/numerator/summary_data-combined.csv'
        inpath_static  = 'data/numerator/static_users-combined.csv'
        
        outpath_static = 'data/numerator/summary_baskets_static-combined.csv'
        outpath_not    = 'data/numerator/summary_baskets_nonstatic-combined.csv'
    else:
        inpath_summary = 'data/numerator/summary_data.csv'
        inpath_static  = 'data/numerator/static_users.csv'
        
        outpath_static = 'data/numerator/summary_baskets_static.csv'
        outpath_not    = 'data/numerator/summary_baskets_nonstatic.csv'
        
    buy = pd.read_csv(inpath_summary)
    static = pd.read_csv(inpath_static)
    
    buy = buy.loc[~pd.isna(buy['TRANSACTION_DATE'])]
    idx = [bool(re.search('^20', x)) for x in buy['TRANSACTION_DATE']]
    buy = buy.loc[idx]
    
    # Add date variable
    buy['date']          = [convert_to_date(x) for x in buy['TRANSACTION_DATE']]
    buy['month']         = [determine_month(x) for x in buy['date']]
    static['START_DATE'] = [convert_to_date(x) for x in static['START_DATE']]
    static['END_DATE']   = [convert_to_date(x) for x in static['END_DATE']]
    
    buy = buy.merge(static, on = 'USER_ID', how = 'left', indicator = True)
    
    buy['in_static'] = 0
    buy.loc[buy['_merge'] == 'both', 'in_static'] = 1
    buy = buy.drop(columns = '_merge')
    
    buy_static = deepcopy(buy.loc[buy['in_static'] == 1])
    buy_not    = deepcopy(buy.loc[buy['in_static'] == 0])
    del buy
    
    buy_static['static_trans'] = (buy_static['date'] >= buy_static['START_DATE']) & \
                                     (buy_static['date'] <= buy_static['END_DATE'])
    buy_not['static_trans'] = 0
    
    buy_static.to_csv(outpath_static, index = False)
    buy_not.to_csv(outpath_not, index = False)


def convert_to_date(x):
    '''
    Convert the string %Y-%m-%d date to a datetime.date object
    '''
    x = re.search('^([0-9]+)-([0-9]+)-([0-9]+)$', x)
    yr  = int(x.group(1))
    mo  = int(x.group(2))
    day = int(x.group(3))
    date_x = dt.date(yr, mo, day)
    return date_x


def determine_month(x):
    yr = x.year
    mo = x.month
    mo = str(mo).zfill(2)
    mo = '%d-%s' % (yr, mo)
    return mo


main()
