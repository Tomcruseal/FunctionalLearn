//laws of map:
//将p经过(a => a) 映射后得到的仍然是p，这就是map的性质
//对于product,首先product应遵守结合律，即
//(a**b)**c ~= a**(b**c)即左右是一个one-to-one
//and onto的映射，唯一不同的是最后结果的tupled层次不一样
//可以通过一组转换函数使得左右两边等号成立
//map 和product在一起也有有趣的性质:

a.map(f)**b.map(g) == (a**b)map{case (a,b) => (f(a),f(b))}

