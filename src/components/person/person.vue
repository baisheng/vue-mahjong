<template>
  <div class="me">
    <div class="top" flex="cross:center">
      <img :src="user_info.iconUrl" alt="">
      <div flex="cross:center dir:top main:center">
        <span>{{user_info.name}}</span>
        <p><span><img src="../../assets/dia.png" alt=""></span>{{user_info.diamond}}</p>
      </div>
    </div>

    <div class="list">
      <div class="item" flex="cross:center box:last" @click='goFeedBack'>
        <div class="text">意见反馈</div>
        <img src="../../assets/arrow.png" alt="">
      </div>
      <div class="item" flex="cross:center box:last" @click='goPro'>
        <div class="text">服务协议</div>
        <img src="../../assets/arrow.png" alt="">
      </div>
  <!--    <div class="item" flex="cross:center box:last">
        <div class="text">关于我们</div>
        <img src="../../assets/arrow_left.png" alt="">
      </div>-->
      <div class="item" flex="cross:center box:last">
        <div class="text" @click="relogin">重新登陆</div>
        <img src="../../assets/arrow.png" alt="">
      </div>
    </div>
    <div class="game-tip" flex="dir:top main:center ">

        <div>抵制不良游戏&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;拒绝盗版游戏</div>
        <div>注意自我保护&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;谨防受骗上当</div>
        <div>适度游戏益脑&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;沉迷游戏伤身</div>
        <div>合理安排时间&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;享受健康生活</div>




    </div>


  </div>
</template>

<script>
  import {mapGetters} from 'vuex'
  export default {
    name: 'person',
    methods: {
      relogin(){
       location.reload()
      },
      goPro(){
        this.$router.push({path:'/proto'})
      },
      goFeedBack(){
         this.$router.push({path:'/feedback'})
      }
    },
    computed:{
      ...mapGetters({
        'user_info':'user_info'
      })
    }
  }
</script>

<style lang="scss" scoped>
  .me {
    margin: .48rem .45rem;

    border: 2px solid #500047;
    background: #ffe8bc;
    border-radius: .16rem;
  }


  .top {
    height: 2rem;
    margin: .2rem;
    border: 2px solid #6a3906;
    border-radius: 8px;
    background: #6a3906;
    box-shadow:  3px 3px 3px rgba(0,0,0,.2) inset;

    > img {
      margin-left: .3rem;
      width: 1.5rem;
      height: 1.5rem;
      border-radius: .2rem;
      border: 2px solid #f4cb3d;
    }
    span {
      margin-left: .4rem;
      color: #fff;
    }

    div {
      height: 100%;
      color: #fff;
      img {
        width: .4rem;
        height: .4rem;
      }
      p {
        margin-top: .4rem;
        img {
          margin-bottom: -.05rem;
          margin-right: .2rem;
        }
      }
    }
  }


  .list {
    color: #fff;
    .item {
      margin: .2rem;
      border: 1px solid #362e2b;
      padding:  .16rem;
      border-radius: 4px;

      .text {
        color: #4c0a0c;
        font-weight: 600;
        margin-left: .1rem;
        font-size: .28rem;
        text-align: left;
      }
      img {
        width: .4rem;
      }
    }
  }
  .game-tip{
    text-align: center;

    margin:.2rem .4rem;
    margin-top: 1.5rem;
    div{
      margin: .1rem 0;
      font-size: .24rem;
      color: #676767;
    }
  }
</style>
