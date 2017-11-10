總體經濟因素對台灣航運業股價的影響

1.選出我認為可能和航運業相關的總體經濟因素

2.調整自變數的落後期數

3.將調整後的自變數轉為趨勢變數，考慮三種不同可能
  -F=1 連續上漲或下跌一期就視為上漲或下跌的趨勢
  -F=2 連續上漲或下跌二期才視為上漲或下跌的趨勢
  -F=3 連續上漲或下跌三期才視為上漲或下跌的趨勢

4.建立Rolling期間
  -Rolling 5年：用1991.1~1995.12的資料，去預測1996.1的股價，依此類推
  -Rolling 10年：用1991.1~2000.12的資料，去預測2001.1的股價，依此類推

5.建立一lasso模型，並將應變數和自變數配適到模型中

6.分別預測不同參數設定下的勝率、並抓出影響較大的變數

7.設置不同門檻進行交易回測

8.初步發現
  -在預測未來一個月股價時，金融變數的影響較大；而預測未來二或三個月股價時，則是經濟變數的影響較大
  -在門檻設為2%、F=2、Rolling 10年、預測未來三個月的狀況下，勝率可達100%
