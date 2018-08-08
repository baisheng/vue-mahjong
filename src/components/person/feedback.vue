<template>
  <div>
    <div class="feedback">
      <h4>选择问题类型</h4>
      <div class="typelist">
        <span :class='{"active":q1[0].isChose}' @click="doQ1(0)">
          意见反馈
        </span>
        <span :class='{"active":q1[1].isChose}' @click="doQ1(1)">
          违法举报
        </span>
        <span :class='{"active":q1[2].isChose}' @click="doQ1(2)">
          游戏建议
        </span>
        <span :class='{"active":q1[3].isChose}' @click="doQ1(3)">
          充值问题
        </span>
        <span :class='{"active":q1[4].isChose}' @click="doQ1(4)">
          扣砖问题
        </span>
        <span :class='{"active":q1[5].isChose}' @click="doQ1(5)">
          掉线问题
        </span>
      </div>
      <!--<span class="myf" @click="myfS=true">我的反馈</span>-->
      <h4>选择游戏类型</h4>
      <div class="type">
        <span :class='{"active":q2[0].isChose}' @click="doQ2(0)">
          象山麻将
        </span>
        <!--<span :class='{"active":q2[1].isChose}' @click="doQ2(1)">-->
          <!--其他麻将-->
        <!--</span>-->
      </div>
      <h4>具体描述</h4>
      <div class="q">
        <textarea v-model="detail"></textarea>
      </div>
      <h4>联系方式</h4>
      <div class="name item">
        <input type="text" v-model="name" placeholder="您的姓名">
      </div>
      <div class="phone item ">
        <input type="text" v-model="phone" placeholder="你的联系方式">
      </div>
      <div style="text-align: center; margin-top: 1rem">
        <a href="javascript:;" class="submit" @click="submit">提交</a>
      </div>

    </div>
    <div class="ok_feedback" v-show='oktip'>
      <div class="close" @click="oktip=false"><img src="../../assets/close.png" alt=""></div>
      <span>问题反馈</span>
      <img src="../../assets/check.png" alt="">
      <p>反馈成功</p>
      <p class="t">我们将尽快处理并回复处理您反馈的问题，谢谢！</p>
      <a href="javascript:;" @click="goHome">返回首页</a>
    </div>
    <!--<div class="bg"></div>-->
    <div class="my_feedback" v-show="myfS">
      <span class="close" @click="myfS=false"><img src="../../assets/close.png" alt=""></span>
      <a href="javascript:;">我要反馈</a>
      <div class="item">
        <div class="time">
          2017-12-12
        </div>
        <div class="intro">
          真好玩
        </div>
      </div>
    </div>
  </div>
</template>

<script>
  import brg from '../../utils/middle'
  export default {
    data() {
      return {
        myfS: false,
        oktip: false,
        q1: [
          {
            isChose: true
          },
          {
            isChose: false
          },
          {
            isChose: false
          },
          {
            isChose: false
          },
          {
            isChose: false
          },
          {
            isChose: false
          },

        ],
        q2: [
          {
            isChose: true
          },
          {
            isChose: false
          },
        ],
        data: [],
        detail:'',
        name:'',
        phone:''
      }
    },
    mounted(){
      brg.$on('protoMsg',(obj)=>{
        let {id, msg} = obj

        if(id==602){
          if(msg.result){
            this.oktip = true
          }
        }
      })
    },
    methods: {
      submit() {
        if(this.name==''){
          return alert('请输入姓名')
        }
        if(this.phone==''){
          return alert('请输入手机号码')
        }
        if(!(/^1[34578]\d{9}$/.test(this.phone))){
          alert("手机号码有误，请重填");
          return false;
        }
        let obj = {
          feedType:this.choseq,
          gameType:this.choseq2,
          content:this.detail,
          name:this.name,
          contact:this.phone
        }

        this.socket.send(601,obj)




      },
      doQ1(index) {

        if(this.q1[index].isChose){
          return
        }else{
          this.q1.forEach((v,k)=>{
            if(k==index){
              v.isChose = true
            }else{
              v.isChose = false
            }
          })
        }
      },
      goHome(){
        this.oktip = false
        this.$router.replace({path:'/room2'})
      },
      doQ2(index){
        if(this.q2[index].isChose){
          return
        }else{
          this.q2.forEach((v,k)=>{
            if(k==index){
              v.isChose = true
            }else{
              v.isChose = false
            }
          })
        }

      }
    },
    computed: {
      choseq() {
        let res = null
        this.q1.find((v, k) => {
//          console.log(v, k)
          if (v.isChose) {
            res = k+1
          }
        })
        return res
      },
      choseq2() {
        let res = null
        this.q2.find((v, k) => {
//          console.log(v, k)
          if (v.isChose) {
            res = k+1
          }
        })
        return res
      }
    }
  }
</script>


<style lang="scss" scoped>
  .feedback {
    position: fixed;
    top: 0;
    left: 0;
    width: 100%;
    height: 100%;
    z-index: 1000;
    background: #ffe9bd;
    padding: .1rem .3rem;
  }

  h3 {
    color: saddlebrown;
    text-align: left;
  }

  h4 {
    display: inline-block;
    border-left: 3px solid saddlebrown;
    color: saddlebrown;
    padding-left: .2rem;
    text-align: left;
    font-size: .26rem;
    margin-top: .2rem;
    margin-bottom: .2rem;
    font-weight: normal;
  }

  .myf {
    position: absolute;
    top: 10px;
    right: 10px;
    color: saddlebrown;
    font-size: .23rem;
    border: 1px solid saddlebrown;
    padding: .05rem .1rem;
  }

  .typelist {
    color: #674941;
    font-size: .24rem;
    &:after {
      content: '';
      display: block;
      clear: both;
    }
    span {
      width: 30%;
      background: saddlebrown;
      color: #fff;
      padding: .13rem;
      margin: 1.66666%;
      text-align: center;
      float: left;
      &.active {
        background: firebrick;
      }
    }
  }

  .type {
    &:after {
      content: '';
      display: block;
      clear: both;
    }
    span {
      width: 20%;

      padding: .13rem;
      margin: 1.66666%;
      float: left;
      font-size: .24rem;
      text-align: center;
      color: #674941;
      font-weight: normal;
      background: saddlebrown;
      color: #fff;
      &.active {
        background: firebrick;
      }
    }
  }

  .q {
    textarea {
      width: 100%;
      background: #fff;
      height: 2rem;
      color: #333;
      border-radius: 10px;
      padding: .2rem;
      line-height: 1.5;
    }
  }

  .item {
    margin-bottom: .15rem;
    input {
      width: 100%;
      background: #fff;
      border-radius: 5px;
      color: #333;
      display: block;
      padding: .1rem .3rem;
      height: .8rem;
    }
  }

  .submit {
    text-decoration: none;
    padding: .2rem .5rem;
    color: #fff;
    background: saddlebrown;
    margin: 0 auto;
    box-shadow: 10px 10px 10px rgba(0, 0, 0, .3);
    border-radius: 5px;
  }

  ::-webkit-input-placeholder {
    font-size: .3rem;
  }

  .ok_feedback {
    position: fixed;
    top: 50%;
    left: 50%;
    transform: translate(-50%, -50%);
    z-index: 99999999;
    width: 90%;
    border: 3px solid saddlebrown;
    background: #ffe9bd;
    text-align: center;
    color: saddlebrown;
    border-radius: .3rem;
    box-shadow: 0 0 12px rgba(0, 0, 0, .8);
    font-size: .3rem;

    span {
      position: absolute;
      top: 10px;
      left: 10px;
      color: saddlebrown;
      font-weight: normal;
    }
    .close {
      position: absolute;
      top: 0;
      right: 0;
      width: .6rem;
      height: .6rem;
      img {
        margin: 0;
        width: 100%;
        height: 100%;
      }
    }
    img {
      width: 4rem;
      margin-top: 1rem;
    }
    .t {
      margin-top: .2rem;
      font-size: .22rem;
    }
    a {
      margin: .4rem auto;
      display: block;
      width: 60%;
      background: saddlebrown;
      padding: .2rem;
      text-decoration: none;
      color: #fff;
      border-radius: .1rem;
      box-shadow: 0 0 10px rgba(0, 0, 0, .3);
    }
  }

  .my_feedback {
    position: fixed;
    top: 50%;
    left: 50%;
    transform: translate(-50%, -50%);
    z-index: 99999999;
    width: 90%;
    border: 3px solid saddlebrown;
    background: #ffe9bd;
    color: saddlebrown;
    border-radius: .3rem;
    box-shadow: 0 0 12px rgba(0, 0, 0, .8);
    font-size: .3rem;
    padding: .5rem .4rem;
    height: 8rem;
    overflow: auto;
    a {
      display: block;
      width: 100%;
      padding: .18rem;
      background: saddlebrown;
      color: #fff;
      text-align: center;
      text-decoration: none;
      margin-bottom: .5rem;
      border-radius: .4rem;
    }
    .close {
      position: absolute;
      top: 0;
      right: 0;
      width: .6rem;
      height: .6rem;
      img {
        width: 100%;
        height: 100%;
      }
    }

  }

  .bg {
    position: fixed;
    top: 0;
    left: 0;
    width: 100%;
    height: 100%;
    background: rgba(0, 0, 0, .87);
    z-index: 99999;
  }
</style>
