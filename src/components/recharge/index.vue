<template>
  <div>

    <div class="content">

      <span class="title">在线充值</span>

      <ul class="list">
        <li flex="main:center cross:center box:justify" >
          <div class="left">
            <img src="../../assets/dia.png" alt="">100
          </div>

          <div class="mid">
            ￥10
          </div>
          <div class="right">
            <a href="javascript:;" @click="goBuy(10)">购买</a>
          </div>
        </li>
        <li flex="main:center cross:center box:justify" >
          <div class="left">
            <img src="../../assets/dia.png" alt="">1000
          </div>

          <div class="mid">
            ￥100
          </div>
          <div class="right">
            <a href="javascript:;" @click="goBuy(100)">购买</a>
          </div>
        </li>



      </ul>

    </div>


  </div>
</template>

<script>
  export default {
    name: 'recharge',
    methods: {
      goBuy(index){
        this.$router.push({path:`/recharge/${index}`})
      }
    }
  }
</script>

<style lang="scss" scoped>

  .content {
    text-align: center;
    margin: .85rem .78rem;
    .title{
      display: block;
      border: 1px solid #000;
      background: #6a3906;
      color: #d2aa78;
      font-size: .32rem;
      padding: .15rem;
      letter-spacing: 2px;
      border: 2px solid #cfa972;
      border-radius: 8px;
      margin-bottom: .4rem;
    }
    .list{

      li{
        border: 1px solid #cfa972;
        margin-bottom: .22rem;
        background: #cfa972;
        height: .8rem;
        padding: .12rem;
        box-shadow: 0 3px 5px  rgba(0, 0, 0, .5) inset;
        border-radius: 8px;
        img{
          width: .36rem;
          margin-left: .2rem;
          margin-right: .1rem;
          margin-bottom:-1px;
        }
        color: #6a3906;
        font-weight: 800;
        a{
          text-decoration: none;
          color: #fff;
          background: #81511c;
          font-weight: 300;
          font-size: .2rem;
          display: inline-block;
          padding:.08rem .25rem;
          border-radius: 5px;
          box-shadow: 0px 0px 10px   rgba(0, 0, 0, .1) inset;
        }
      }
    }

  }
</style>
