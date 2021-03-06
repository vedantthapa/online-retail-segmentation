-- Customer data
SELECT CustomerID,
        sum(Quantity) as TOTAL_PRODS,
        count(distinct StockCode) as BASKETS,
        sum(Quantity*UnitPrice) as TOTALREVENUE,
        count(distinct InvoiceNo) as NO_OF_VISITS,
        (select sum(Quantity*UnitPrice)/count(distinct InvoiceNo)) as AVG_SPEND,
        DATEDIFF("2011-12-31 00:00:00", max(InvoiceDateTime)) as DAYS_AFTER_LAST_PURCHASE
FROM OnlineRetail
WHERE UnitPrice>0
        AND CustomerID != 0
    AND Country='United Kingdom'
GROUP BY CustomerID
ORDER BY 1;

-- Product data
SELECT StockCode,
    COUNT(DISTINCT CustomerID) as CUST_COUNT,
    SUM(Quantity*UnitPrice) as TOTALPRODREVENUE,
    COUNT(DISTINCT InvoiceNo) as POPULARITY,
    (SELECT SUM(Quantity*UnitPrice)/SUM(Quantity)) as AVG_REVENUE
FROM OnlineRetail
WHERE Country='United Kingdom'
    AND CustomerID != 0
GROUP BY StockCode
ORDER BY TOTALPRODREVENUE DESC;
