library(recommenderlab)

# Jester online recommender system�翡�� 1999�� 4������ 2003�� 5�� ��
# 100���� ��ǰ�� ���Ͽ� 5000���� ������ �ű�
# ���� -10~10

data(Jester5k)
head(as(Jester5k, "matrix"))

# ���� ����
# method:    UBCF(�� �߽�), IBCF(��ǰ �߽�)
# type:     ratings(���� ����), topNList(��ǰ ��õ)
r = Recommender(Jester5k[1:1000], method="UBCF")
pr = predict(r, Jester5k[1001:1002], type="ratings")
as(pr, "matrix")

# ��ǰ ��õ
ptype = predict(r, Jester5k[1001:1002], n=5)
as(ptype, "list")

