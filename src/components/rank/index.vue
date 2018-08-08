<template>
  <div class="rank" v-if="paiju_record">
    <div class="top">
      <div class="nav" flex="cross:center box:mean ">
        <span :class="{ active: isCount }" flex="main:center cross:center" @click="isCount=true">牌局统计</span>
        <span :class="{ active: !isCount }" flex="main:center cross:center" @click="isCount=false">战绩统计</span>
      </div>
    </div>
    <div v-show="isCount" v-if="paiju_record">
      <div class="middle" flex=" cross:center box:mean">
        <div>总牌局:{{paiju_record.allTimes}}</div>
        <div style="float:right"> 总胜率:{{paiju_record.allWinProb | format1}}%</div>
      </div>
      <div class="list">
        <div class="empty" style="display: none">
          你还没有牌局
        </div>
        <div class="content">
          <div class="item" flex="main:center cross:center box:mean" v-for="n in paiju_record.list" @click="goB()">
            <div class="p1">{{n.type == '1' ? '象山麻将' : '开发中麻将'}}</div>
            <div class="p1">局数
              <span>{{n.totalTimes}}</span>
            </div>
            <div class="p1">胜率
              <span>{{n.winProb | format1}}%</span>
            </div>
            <div class="p1">
              <a href="javascript:;">查看战绩</a>
            </div>
          </div>
        </div>
      </div>
      <div class="tip">注: 战绩只保留一个月内的数据</div>
    </div>

    <div v-show="!isCount" v-if="rank_record">
      <div class="middle" flex="main:center cross:center box:mean">
        <span>总牌局: {{paiju_record.allTimes}}</span>
        <span>胜率: {{paiju_record.allWinProb | format1}}%</span>
      </div>
      <div class="list">
        <div class="empty" style="display: none;">
          你还没有战绩
        </div>

        <div class="content2">
          <div class="item" flex="main:center cross:center box:first" v-for="n in rank_record" @click="goD(n.info.id)">
            <div class="p1">{{n.info.startTime | formatMD}}</div>
            <div class="p2">
              <img :src="user_info.iconUrl" alt="">
            </div>
            <div class="p3">
              <p>{{n.info.startTime | formatHS}} 来自 {{n.info.type == '1' ? '象山麻将' : '开发中麻将'}}</p>
              <p>{{n.info.totalTime | formatS}}分钟局</p>
            </div>
            <div class="p4">
              &nbsp;&nbsp;
              <span>
                <img class="icon-l" src="../../assets/arrow.png">
              </span>
            </div>
          </div>
        </div>
      </div>
      <div class="tip">注: 战绩只保留一个月内的数据</div>
    </div>

  </div>
</template>

<script>
  import {mapGetters} from 'vuex'
  import brg from '../../utils/middle'

  const moment = require("moment")
  export default {
    data() {
      return {
        isCount: false
      }
    },
    beforeMount() {
      this.socket.send(501)
      this.socket.send(503)
      this.socket.send(505)
      brg.$on('protoMsg', (obj) => {
        let {id, msg} = obj
        if (id == 502) {
          this.$store.dispatch('fetchPaijuRecord', msg)
        }

        if (id == 504) {
          this.$store.dispatch('fetchRankRecord', msg.list)
        }

      })

  },
  mounted()
  {



  }
  ,
  computed: {
  ...
    mapGetters({
      'rank_record': 'rank_record',
      'my_record': 'my_record',
      'user_info': 'user_info',
      'paiju_record': 'paiju_record',
    })
  }
  ,
  filters: {
    format1(val)
    {
      let tmp = (val * 100).toFixed(2)
      return tmp
    }
  ,
    formatMD(val)
    {
      return moment(val * 1000).format('MM-DD')
    }
  ,
    formatHS(val)
    {
      return moment(val * 1000).format('HH:mm')
    }
  ,
    formatS(val)
    {
      return val / 60
    }
  ,

  }
  ,
  methods: {
    goD(id)
    {
      console.log(id)
      this.$router.push({path: `/rankd/${id}`})
    }
  ,
    goB()
    {
      this.isCount = false
    }
  ,
    initRecordData()
    {


    }
  }
  }
</script>

<style scoped lang="scss">
  .rank {
    border: 2px solid #500047;
    margin: .5rem .46rem;
    background: #ffe8bc;
    padding: .24rem;
    border-radius: 8px;
    height: 9.6rem;
  }

  .top {
    .nav {
      height: .7rem;
      margin-bottom: .4rem;

      span {
        height: 100%;
        font-size: .26rem;
        border: 2px solid #81511c;
        background: #d7c689;

        border-radius: 5px;
        color: #6a3906;

        box-shadow: 0 0 8px rgba(0, 0, 0, .3) inset;
        &.active {
          background: #6a3906;
          color: #fff;
        }
        &:last-of-type {
          margin-left: .1rem;
        }
        &:first-of-type {
          margin-right: .1rem;
        }
      }
    }
  }

  .middle {
    height: .7rem;
    background: #6a3906;
    color: #cfa972;
    border: 2px solid #cfa972;
    border-radius: 5px;
    box-shadow: 0 0 8px rgba(0, 0, 0, .3) inset;
    text-align: left;
    padding-left: .2rem;
    font-size: .24rem;
    margin-bottom: .4rem;
    span {
      &:last-of-type {
        text-align: right;
        margin-right: .2rem;
      }
    }
  }

  .list {
    .empty {
      font-size: .32rem;
      color: #6a3906;
      font-weight: bold;
    }
    .content2 {
      height: 6.2rem;
      border-radius: 8px;
      font-size: .24rem;
      color: #4b453d;
      font-weight: 600;
      padding: .2rem;
      overflow: auto;
      > div {
        margin-bottom: .2rem;
      }
      img {
        width: .9rem;
        height: .9rem;
        border: 2px solid #ffd015;
        border-radius: .45rem;
      }
      .icon-l {
        border: none;
        width: .4rem;
        height: .35rem;
        border-radius: 0;
      }
      .p1 {
        text-align: center;
      }
      .p2 {
        width: .7rem;
      }
      .p3 {
        width: 1.56rem;
        text-align: left;
      }
      .p4 {
        font-size: .34rem;
      }
    }
    .content {
      padding-top: .2rem;
      background: #a6937c;
      height: 6.2rem;
      border: 2px solid #cfa972;
      border-radius: 8px;
      box-shadow: 0 0 8px rgba(0, 0, 0, .3) inset;
      font-size: .26rem;
      .item {
        padding: .2rem;
        span {
          color: #333;
        }
        a {
          text-decoration: none;
          border: none;
          color: #fff;
          display: inline-block;
          padding: .1rem .2rem;
          background: #f39800;
          text-shadow: 0 0 5px #999;
          font-size: .2rem;
          box-shadow: 0 0 5px rgba(0, 0, 0, .1);
        }
      }
    }
  }

  .tip {
    color: #734411;
    text-align: left;
    margin: .3rem .1rem;
    font-size: .28rem;
    font-weight: bold;
  }

  .space {
    height: 2rem;
  }
</style>
