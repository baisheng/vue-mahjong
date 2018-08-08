<template>
  <div>

    <div class="content">

      <div class="top" flex="main:center cross:center box:first">
        <div class="img" flex="main:center cross:center box:first">
          <img :src="user_info.iconUrl" alt="">
        </div>
        <div class="text">
          <p>砖石充值</p>
          <div class="b">
            <img src="../../assets/dia.png" alt="">{{user_info.diamond}} <a href="javascript:;">充值</a>
          </div>
        </div>
      </div>

      <div class="detail">
        <h5>订单的信息</h5>
        <div>充值账号：{{user_info.name}}</div>
        <div>商品名称：{{diamo}}&nbsp;钻</div>
        <div>商品价格：{{$route.params.id}} RMB</div>
      </div>

      <div class="type" flex="main:center cross:center box:mean">
        <a href="javascript:;" @click="toPay">微信支付</a>
      </div>

      <div class="intro">
        <h3>下单说明</h3>
        <p>1、钻石商品为虚拟物品，一经售出，概不退还。</p>
        <p>2、使用“充值卡/游戏卡”充值的用户，请确保卡面额与您的下单金额一致。</p>
        <p>3、支付遇到问题？请点此处提交客服咨询处理。</p>
      </div>

    </div>


  </div>
</template>

<script>
  import {mapGetters} from 'vuex'
  import {config} from '../../utils/config'
  const md5 = require('../../utils/md5')
  import $ from 'jquery'
  export default {
    name: 'recharge',
    mounted() {
      let id = this.$route.params.id
      console.log(id)
    },
    methods: {
      toPay(){
        let time =  (new Date()).valueOf();
        let wecha_id = this.myself.wecha_id;
        let key1 = config.payKey;
        let key = md5(time+key1);
        let price = this.$route.params.id;


        $.ajax({
          url:"http://s.h5taotao.com/index2.php?g=Wap&m=Jiekouall&a=wxpay",
          type:'POST',
          data:{
            time:time,
            wecha_id:wecha_id,
            key:key,
            price:price
          },
          dataType:'JSON',
          success:function (data) {
            if(data.status){

              window.location.href = `http://w.h5taotao.com/h5/majhong1wxpay/example/jsapi.php?order=${data.order}&wecha_id=${wecha_id}&price=${price}`
            }
          }
        })
      }
    },
    computed: {
      ...mapGetters({
        'user_info': 'user_info'
      }),
      diamo() {
        let val = parseInt(this.$route.params.id)
        let res = 100
        switch (val) {
          case 10:
            res = 100
            break;
          case 100:
            res = 1000
            break;
          case 50:
            res = 5400
            break;
          case 100:
            res = 11000
            break;
          default:
            res = 10000
        }
        return res
      }
    }
  }
</script>

<style lang="scss" scoped>

  .content {
    margin: .48rem .45rem;
    background: #ffe8bc;
    border: 1px solid #500047;
    border-radius: 8px;
    padding: .32rem .24rem;

    .top {
      border: 2px solid #cfa972;
      background: #6a3906;
      border-radius: 8px;
      height: 1.8rem;
      box-shadow: 0 5px 8px rgba(0, 0, 0, .4) inset;
      margin-bottom: .34rem;
      .img {
        width: 1.8rem;
        height: 100%;
        img {
          width: 1.2rem;
          height: 1.2rem;
          border: 2px solid #f1c539;
          border-radius: 5px;
        }
      }
      .text {
        text-align: left;
        color: #d2aa78;
        font-size: .28rem;
        .b {
          margin-top: .4rem;
          color: #fff;
          font-weight: 400;

          img {
            width: .35rem;
            margin-right: .12rem;
          }

          a {
            display: inline-block;
            padding: .02rem .12rem;
            text-decoration: none;
            color: #fff;
            background: #e60012;
            display: inline-block;
            font-size: .24rem;
            margin-left: .4rem;
            border-radius: .18rem;
          }

        }
      }

    }
    .detail {
      text-align: left;
      background: #cfa972;
      box-shadow: 3px 5px 3px rgba(0, 0, 0, .2) inset;
      padding: .15rem .22rem;
      color: #6a3906;
      border: 2px solid #cfa972;
      border-radius: 8px;
      margin-bottom: .34rem;
      h5 {
        text-align: center;
        font-size: .34rem;
        margin-bottom: .1rem;
      }
      div {
        font-weight: 600;
        font-size: .28rem;
        line-height: 2;
      }
    }
    .type {
      margin-bottom: .3rem;
      a {
        padding: .2rem;
        background: #81511c;
        line-height: .52rem;
        color: #fff;
        text-decoration: none;
        border-radius: 5px;
        font-size: .34rem;
        box-shadow: 3px 3px 5px rgba(0, 0, 0, .1);
        margin: .1rem;
        text-align: center;
        &:last-of-type {
          background: #8a8000;

        }
      }
    }
    .intro {
      h3 {
        font-size: .32rem;
        margin-bottom: .1rem;
      }
      line-height: 1.5;
      font-size: .3rem;
      text-align: left;
      color: #333;
    }

  }
</style>
